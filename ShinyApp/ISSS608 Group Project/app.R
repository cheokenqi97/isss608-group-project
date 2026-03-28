library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(scales)
library(plm)
library(lmtest)
library(sandwich)

# =========================
# Load data
# =========================
analysis_df <- readRDS("data/analysis_df.rds") %>% arrange(year)
tfr_df <- readRDS("data/tfr_df.rds") %>% arrange(year)
asfr_df <- readRDS("data/asfr_df.rds") %>% arrange(year)
series_key <- readRDS("data/series_key.rds")

# =========================
# Helper functions
# =========================
extract_ccf_tbl <- function(x, y, x_name, y_name, lag_max = 5) {
  ccf_obj <- ccf(x, y, lag.max = lag_max, plot = FALSE, na.action = na.omit)
  tibble(
    lag = as.numeric(ccf_obj$lag),
    correlation = as.numeric(ccf_obj$acf),
    pair = paste(x_name, "vs", y_name)
  )
}

get_peak_ccf <- function(df, pair_name) {
  df %>%
    slice_max(order_by = abs(correlation), n = 1, with_ties = FALSE) %>%
    transmute(
      Pair = pair_name,
      `Peak Lag` = lag,
      `Peak Correlation` = correlation
    )
}

coef_table <- function(model) {
  ct <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  tibble(
    term = rownames(ct),
    estimate = unname(ct[, 1]),
    std_error = unname(ct[, 2]),
    statistic = unname(ct[, 3]),
    p_value = unname(ct[, 4])
  )
}

fmt_p <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}

# =========================
# UI
# =========================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML(" 
      body { background-color: #f3f4fa; font-family: Arial, sans-serif; }
      .main-title {
        background: linear-gradient(90deg, #111827, #000000);
        color: white; text-align: center; font-size: 32px; font-weight: 700;
        padding: 14px; border-radius: 10px; margin-bottom: 20px; letter-spacing: 0.3px;
      }
      .box-card {
        background: white; border-radius: 16px; padding: 24px; margin-bottom: 20px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.06);
      }
      .intro-card { background: linear-gradient(135deg, #f8fafc, #eef2ff); border-left: 6px solid #4f46e5; }
      .chart-card h3 { margin-bottom: 8px; color: #1f2937; font-weight: 700; }
      .chart-desc { font-size: 14px; color: #6b7280; margin-bottom: 15px; line-height: 1.6; }
      .panel-title { font-size: 22px; font-weight: 700; color: #1f2937; margin-bottom: 8px; }
      .panel-desc { font-size: 14px; color: #6b7280; margin-bottom: 18px; line-height: 1.6; }
      .metric-card {
        border-radius: 18px; padding: 30px 20px; text-align: center;
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.08); min-height: 200px;
        transition: transform 0.2s ease, box-shadow 0.2s ease; display: flex;
        flex-direction: column; justify-content: center; align-items: center;
        background-color: white;
      }
      .metric-card:hover { transform: translateY(-4px); box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12); }
      .metric-card.divorce { background-color: #ffe4e6; border-top: 5px solid #ef4444; }
      .metric-card.tfr { background-color: #dbeafe; border-top: 5px solid #2563eb; }
      .metric-card.marriage { background-color: #dcfce7; border-top: 5px solid #10b981; }
      .metric-icon { font-size: 28px; margin-bottom: 10px; }
      .metric-title { font-size: 18px; font-weight: 600; color: #374151; margin-bottom: 10px; line-height: 1.3; }
      .metric-value { font-size: 36px; font-weight: 700; color: #111827; line-height: 1.1; margin-bottom: 6px; }
      .metric-subtext { font-size: 15px; color: #6b7280; margin: 0; }
      .feature-row { display: flex; gap: 24px; margin-top: 10px; align-items: stretch; }
      .feature-col { flex: 1; display: flex; }
      .feature-card {
        background: white; border-radius: 18px; padding: 28px 22px; text-align: center;
        box-shadow: 0 4px 14px rgba(0,0,0,0.06); min-height: 320px; width: 100%;
        display: flex; flex-direction: column; justify-content: flex-start; align-items: center;
      }
      .feature-icon { font-size: 54px; color: #2563eb; margin-bottom: 18px; }
      .feature-title {
        font-size: 22px; font-weight: 700; color: #1f2937; line-height: 1.25;
        min-height: 56px; display: flex; align-items: center; justify-content: center;
        margin-bottom: 12px; text-align: center;
      }
      .feature-desc { font-size: 15px; color: #6b7280; line-height: 1.6; margin: 0; }
      .sidebar-card {
        background: white; border-radius: 12px; padding: 18px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08); margin-bottom: 20px;
      }
      .note-box {
        background: #f8fafc; border-left: 4px solid #64748b; padding: 14px 16px;
        border-radius: 10px; font-size: 14px; color: #334155; line-height: 1.6;
      }
      @media (max-width: 991px) { .feature-row { flex-direction: column; } }
    "))
  ),
  
  div(class = "main-title", "Singapore Fertility Trend Dashboard"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Home",
      div(
        class = "box-card",
        h2("Welcome to the Dashboard!"),
        p("Singapore’s total fertility rate has fallen to a historic low, raising concerns about population ageing and the long-term sustainability of the labour force. Fertility trends are often shaped by a wider set of demographic and social factors, including marriage patterns, divorce trends, and delayed childbearing across different age groups."),
        p("This dashboard focuses on exploratory and explanatory analysis rather than prediction. It first uses cross-correlation to examine the overall lag pattern between fertility, marriage, and divorce. It then uses fixed-effects panel regression on age-group-by-year data to examine these relationships more formally."),
        p("Together, the Trend & EDA, Cross-Correlation Analysis, and Panel Regression Analysis tabs provide a clearer view of both the temporal patterns and age-group differences present in the data.")
      ),
      
      div(
        class = "box-card",
        h3("At a Glance"), br(),
        fluidRow(
          column(4,
                 div(class = "metric-card divorce",
                     div(class = "metric-icon", icon("heart-crack")),
                     div(class = "metric-title", "Latest Divorce Indicator"),
                     div(class = "metric-value", round(tail(na.omit(analysis_df$divorce), 1), 1)),
                     p(class = "metric-subtext", "latest observed value")
                 )),
          column(4,
                 div(class = "metric-card tfr",
                     div(class = "metric-icon", icon("child")),
                     div(class = "metric-title", "Latest TFR"),
                     div(class = "metric-value", round(tail(na.omit(analysis_df$tfr), 1), 2)),
                     p(class = "metric-subtext", "births per woman")
                 )),
          column(4,
                 div(class = "metric-card marriage",
                     div(class = "metric-icon", icon("ring")),
                     div(class = "metric-title", "Latest Marriage Indicator"),
                     div(class = "metric-value", round(tail(na.omit(analysis_df$marriage), 1), 1)),
                     p(class = "metric-subtext", "latest observed value")
                 ))
        )
      ),
      
      div(
        class = "box-card",
        h3("Main Analytical Sections"),
        p("The dashboard is organised into three main sections. Each section focuses on a different part of the analysis, from broad trend exploration to time-aware statistical analysis."),
        div(
          class = "feature-row",
          div(class = "feature-col",
              div(class = "feature-card",
                  div(class = "feature-icon", icon("chart-line")),
                  div(class = "feature-title", "Trend & EDA"),
                  p(class = "feature-desc", "This section explores the main demographic trends in the dataset, including total fertility rate, marriage, divorce, and age-specific fertility rates. It helps users identify broad patterns, turning points, and long-term shifts before moving into deeper time-aware analysis."))),
          div(class = "feature-col",
              div(class = "feature-card",
                  div(class = "feature-icon", icon("project-diagram")),
                  div(class = "feature-title", "Cross-Correlation Analysis"),
                  p(class = "feature-desc", "This section explores how fertility, marriage, and divorce move relative to one another across different lags. It highlights the direction of co-movement, the relative strength of the cross-correlation, and the timing at which the clearest pattern appears."))),
          div(class = "feature-col",
              div(class = "feature-card",
                  div(class = "feature-icon", icon("chart-bar")),
                  div(class = "feature-title", "Panel Regression Analysis"),
                  p(class = "feature-desc", "This section uses a fixed-effects panel model on age-group-by-year data to estimate the direction, coefficient size, and statistical significance of the relationship between fertility, marriage, and divorce more formally.")))
        )
      )
    ),
    
    tabPanel(
      "Trend & EDA",
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-card",
            h3("Explore the Data", class = "panel-title"),
            p(class = "panel-desc", "Use the filter below to explore patterns across different years."),
            sliderInput(
              "eda_year", "Year Range",
              min = min(analysis_df$year, na.rm = TRUE),
              max = max(analysis_df$year, na.rm = TRUE),
              value = c(min(analysis_df$year, na.rm = TRUE), max(analysis_df$year, na.rm = TRUE)),
              sep = ""
            )
          )
        ),
        column(
          width = 9,
          div(class = "box-card intro-card",
              h2("Trend & Exploratory Data Analysis"),
              p("This section provides an interactive overview of Singapore’s key demographic trends. The charts below allow users to examine long-term fertility changes, compare related demographic indicators, and identify whether fertility has shifted towards older age groups over time.")),
          div(class = "box-card chart-card",
              h3("1. Total Fertility Rate Over Time"),
              p(class = "chart-desc", "This chart illustrates the long-term decline in Singapore’s total fertility rate over the selected period."),
              plotOutput("tfr_trend_plot", height = "400px")),
          div(class = "box-card chart-card",
              h3("2. Fertility, Marriage, and Divorce Trends"),
              p(class = "chart-desc", "This chart compares the overall trends in fertility, marriage, and divorce indicators over time."),
              plotOutput("mar_div_plot", height = "550px")),
          div(class = "box-card chart-card",
              h3("3. Age-Specific Fertility Rates"),
              p(class = "chart-desc", "This chart shows fertility patterns across age groups and helps reveal whether childbearing has shifted towards older ages."),
              plotOutput("asfr_plot", height = "500px")),
          div(class = "box-card chart-card",
              h3("4. Fertility Postponement as a Composition Shift"),
              p(class = "chart-desc", "This chart shows how the share of fertility has gradually shifted from younger age groups to older age groups over time."),
              plotOutput("asfr_share_plot", height = "500px")),
          div(class = "box-card chart-card",
              h3("5. Scatterplots of TFR Against Marriage and Divorce"),
              p(class = "chart-desc", "These scatterplots summarise the same-year relationship between total fertility rate and the marriage and divorce indicators."),
              fluidRow(
                column(6, plotOutput("scatter_mar_plot", height = "350px")),
                column(6, plotOutput("scatter_div_plot", height = "350px"))
              ))
        )
      )
    ),
    
    tabPanel(
      "Cross-Correlation Analysis",
      fluidRow(
        column(
          3,
          div(
            class = "sidebar-card",
            h3("Explore the Lag Pattern", class = "panel-title"),
            p(class = "panel-desc", "Choose an indicator and lag window to compare how fertility co-moves with marriage and divorce over time."),
            selectInput("ccf_var", "Indicator:", choices = c("Marriage", "Divorce", "Both"), selected = "Both"),
            sliderInput("ccf_lag", "Maximum Lag:", min = 0, max = 5, value = 5, step = 1),
            sliderInput(
              "ccf_year", "Year Range:",
              min = min(analysis_df$year, na.rm = TRUE),
              max = max(analysis_df$year, na.rm = TRUE),
              value = c(min(analysis_df$year, na.rm = TRUE), max(analysis_df$year, na.rm = TRUE)),
              sep = ""
            )
          )
        ),
        column(
          9,
          div(class = "box-card intro-card",
              h2("Cross-Correlation Analysis"),
              p("This section explores how fertility, marriage, and divorce move relative to one another across different lags. It highlights the direction of co-movement, the relative strength of the cross-correlation, and the timing at which the clearest pattern appears.")),
          div(class = "box-card chart-card",
              h3("1. Cross-Correlation Plot"),
              p(class = "chart-desc", "The plot shows how the cross-correlation changes across the selected lag window. When both indicators are selected, the two lag patterns are shown side by side for comparison."),
              plotOutput("ccf_plot", height = "430px")),
          div(class = "box-card chart-card",
              h3("2. Lag Values Table"),
              p(class = "chart-desc", "This table shows the exact cross-correlation values across lags for the selected indicator(s)."),
              DTOutput("ccf_table")),
        )
      )
    ),
    
    tabPanel(
      "Panel Regression Analysis",
      fluidRow(
        column(
          3,
          div(
            class = "sidebar-card",
            h3("Explore the Panel Model", class = "panel-title"),
            p(class = "panel-desc", "Use the filter below to explore across different years."),
            sliderInput(
              "panel_year", "Year Range:",
              min = min(asfr_df$year, na.rm = TRUE),
              max = max(asfr_df$year, na.rm = TRUE),
              value = c(min(asfr_df$year, na.rm = TRUE), max(asfr_df$year, na.rm = TRUE)),
              sep = ""
            )
          )
        ),
        column(
          9,
          div(class = "box-card intro-card",
              h2("Panel Regression Analysis"),
              p("The cross-correlation analysis identified lag 0 as the clearest timing pattern for both marriage and divorce. The panel regression was then used to examine the same-year relationship more formally, allowing us to estimate the coefficient size and statistical significance."),
              div(class = "note-box",
                  HTML("<b>Note:</b> Fixed-effects panel regression was used as the main model to quantify the same-year relationship. Random-effects was used as a comparison, and since it gave a similar conclusion, keeping the fixed-effects model as the main result is reasonable."))),
          div(class = "box-card chart-card",
              h3("1. Fixed-Effects Coefficient Plot"),
              p(class = "chart-desc", 
                "This chart shows the estimated same-year coefficients from the fixed-effects model for marriage and divorce."),
              plotOutput("panel_coef_plot", height = "400px"),
              br(),
              uiOutput("panel_interpretation")),
          div(class = "box-card chart-card",
              h3("2. Fixed-Effects Regression Summary Table"),
              p(class = "chart-desc", "This table reports the summary results."),
              DTOutput("panel_regression_table"))

        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # ---- Trend & EDA ----
  eda_analysis <- reactive({
    analysis_df %>%
      filter(year >= input$eda_year[1], year <= input$eda_year[2]) %>%
      arrange(year)
  })
  
  eda_tfr <- reactive({
    tfr_df %>%
      filter(year >= input$eda_year[1], year <= input$eda_year[2]) %>%
      arrange(year)
  })
  
  eda_asfr <- reactive({
    asfr_df %>%
      filter(year >= input$eda_year[1], year <= input$eda_year[2]) %>%
      filter(!is.na(age_band)) %>%
      arrange(year, age_band)
  })
  
  output$tfr_trend_plot <- renderPlot({
    df <- eda_tfr()
    validate(need(nrow(df) > 0, "No TFR data available for the selected year range."))
    
    y_col <- if ("value" %in% names(df)) "value" else names(df)[2]
    
    ggplot(df, aes(x = year, y = .data[[y_col]])) +
      geom_line(linewidth = 1, color = "#2563eb") +
      geom_point(size = 2, color = "#2563eb") +
      labs(title = "Singapore Total Fertility Rate Over Time", x = "Year", y = "Births per woman") +
      theme_minimal()
  })
  
  output$mar_div_plot <- renderPlot({
    df <- eda_analysis()
    validate(need(all(c("year", "tfr", "marriage", "divorce") %in% names(df)), "Required columns are missing in analysis_df."))
    
    plot_df <- df %>%
      select(year, tfr, marriage, divorce) %>%
      pivot_longer(cols = c(tfr, marriage, divorce), names_to = "series", values_to = "value") %>%
      mutate(series = recode(series,
                             tfr = "Total Fertility Rate",
                             marriage = "Marriage Indicator",
                             divorce = "Divorce Indicator")) %>%
      filter(!is.na(value))
    
    plot_df <- plot_df %>%
      filter(year >= 1980)
    
    ggplot(plot_df, aes(x = year, y = value)) +
      geom_line(linewidth = 1, color = "#2C3E50") +
      geom_point(size = 1.5, color = "#2C3E50") +
      facet_wrap(~series, scales = "free_y", ncol = 1) +
      scale_x_continuous(
        breaks = seq(1980, max(plot_df$year, na.rm = TRUE), by = 5)
      ) +
      labs(title = "Fertility, Marriage and Divorce Trends", x = "Year", y = "Value") +
      theme_minimal()
  })
  
  output$asfr_plot <- renderPlot({
    df <- eda_asfr() %>% mutate(age_band = as.character(age_band))
    validate(need(nrow(df) > 0, "No ASFR data available for the selected year range."))
    
    ggplot(df, aes(x = year, y = value, colour = age_band)) +
      geom_line(linewidth = 0.9) +
      labs(title = "Age-Specific Fertility Rates", x = "Year", y = "Births per 1,000 females", colour = "Age band") +
      theme_minimal()
  })
  
  output$asfr_share_plot <- renderPlot({
    asfr_share_df <- eda_asfr() %>%
      group_by(year) %>%
      mutate(total_asfr = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(share = if_else(total_asfr > 0, value / total_asfr, NA_real_)) %>%
      filter(!is.na(share))
    
    validate(need(nrow(asfr_share_df) > 0, "No valid ASFR share data available."))
    
    ggplot(asfr_share_df, aes(x = year, y = share, fill = age_band)) +
      geom_area() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Fertility Postponement as a Composition Shift", x = "Year", y = "Share of age-specific fertility", fill = "Age band") +
      theme_minimal()
  })
  
  output$scatter_mar_plot <- renderPlot({
    df <- eda_analysis() %>% drop_na(tfr, marriage)
    validate(need(nrow(df) > 1, "Not enough data to draw the TFR vs Marriage scatterplot."))
    
    ggplot(df, aes(x = marriage, y = tfr)) +
      geom_point(size = 2, color = "#10b981") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#065f46") +
      labs(title = "TFR vs Marriage", x = "Marriage indicator", y = "TFR") +
      theme_minimal()
  })
  
  output$scatter_div_plot <- renderPlot({
    df <- eda_analysis() %>% drop_na(tfr, divorce)
    validate(need(nrow(df) > 1, "Not enough data to draw the TFR vs Divorce scatterplot."))
    
    ggplot(df, aes(x = divorce, y = tfr)) +
      geom_point(size = 2, color = "#ef4444") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#991b1b") +
      labs(title = "TFR vs Divorce", x = "Divorce indicator", y = "TFR") +
      theme_minimal()
  })
  
  # ---- Cross-Correlation ----
  ccf_data <- reactive({
    analysis_df %>%
      filter(year >= input$ccf_year[1], year <= input$ccf_year[2]) %>%
      select(year, tfr, marriage, divorce) %>%
      drop_na() %>%
      arrange(year)
  })
  
  ccf_tables <- reactive({
    df <- ccf_data()
    validate(need(nrow(df) >= 6, "Not enough yearly observations to compute cross-correlation over the selected lag window."))
    
    lag_max <- input$ccf_lag
    marriage_tbl <- extract_ccf_tbl(df$tfr, df$marriage, "TFR", "Marriage", lag_max = lag_max)
    divorce_tbl <- extract_ccf_tbl(df$tfr, df$divorce, "TFR", "Divorce", lag_max = lag_max)
    
    list(marriage = marriage_tbl, divorce = divorce_tbl)
  })
  
  output$ccf_plot <- renderPlot({
    tbls <- ccf_tables()
    
    plot_df <- switch(
      input$ccf_var,
      "Marriage" = tbls$marriage %>% mutate(series = "TFR vs Marriage"),
      "Divorce" = tbls$divorce %>% mutate(series = "TFR vs Divorce"),
      "Both" = bind_rows(
        tbls$marriage %>% mutate(series = "TFR vs Marriage"),
        tbls$divorce %>% mutate(series = "TFR vs Divorce")
      )
    )
    
    ggplot(plot_df, aes(x = lag, y = correlation, colour = series)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(xintercept = 0, linetype = 3, colour = "grey50") +
      labs(title = "Cross-Correlation Across Lags", x = "Lag (Years)", y = "Cross-Correlation", colour = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"), legend.position = "top")
  })
  
  output$ccf_table <- renderDT({
    tbls <- ccf_tables()
    out_tbl <- switch(
      input$ccf_var,
      "Marriage" = tbls$marriage,
      "Divorce" = tbls$divorce,
      "Both" = bind_rows(tbls$marriage, tbls$divorce)
    ) %>%
      mutate(correlation = round(correlation, 3))
    
    datatable(out_tbl, rownames = FALSE,
              options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE))
  })
  

  # ---- Panel Regression (Fixed Effects only) ----
  panel_df <- reactive({
    asfr_df %>%
      filter(year >= input$panel_year[1], year <= input$panel_year[2]) %>%
      transmute(
        year,
        age_group = as.character(age_band),
        asfr = value
      ) %>%
      left_join(analysis_df %>% select(year, marriage, divorce), by = "year") %>%
      drop_na(age_group, year, asfr, marriage, divorce) %>%
      arrange(age_group, year)
  })
  
  panel_model <- reactive({
    df <- panel_df()
    validate(need(nrow(df) > 10, "Not enough panel observations available for the selected year range."))
    validate(need(length(unique(df$age_group)) > 1, "At least two age groups are needed for panel regression."))
    
    pdata <- pdata.frame(df, index = c("age_group", "year"))
    fe_model <- plm(asfr ~ marriage + divorce, data = pdata, model = "within")
    
    list(
      df = df,
      fe = fe_model,
      fe_tbl = coef_table(fe_model)
    )
  })
  
  output$agegroup_asfr_plot <- renderPlot({
    df <- panel_df()
    
    ggplot(df, aes(x = year, y = asfr, colour = age_group, group = age_group)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      labs(
        title = "ASFR Trends Across Age Groups",
        x = "Year",
        y = "ASFR",
        colour = "Age Group"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
  
  output$panel_regression_table <- renderDT({
    mods <- panel_model()
    
    show_tbl <- mods$fe_tbl %>%
      filter(term %in% c("marriage", "divorce")) %>%
      mutate(
        term = recode(term, marriage = "Marriage", divorce = "Divorce"),
        estimate = round(estimate, 3),
        std_error = round(std_error, 3),
        statistic = round(statistic, 3),
        p_value = fmt_p(p_value)
      ) %>%
      select(term, estimate, std_error, statistic, p_value)
    
    datatable(
      show_tbl,
      rownames = FALSE,
      options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE)
    )
  })
  
  output$panel_coef_plot <- renderPlot({
    mods <- panel_model()
    
    coef_plot_df <- mods$fe_tbl %>%
      filter(term %in% c("marriage", "divorce")) %>%
      mutate(term = recode(term, marriage = "Marriage", divorce = "Divorce"))
    
    if (!is.null(input$panel_vars) && length(input$panel_vars) > 0) {
      coef_plot_df <- coef_plot_df %>% filter(term %in% input$panel_vars)
    }
    
    validate(need(nrow(coef_plot_df) > 0, "Select at least one variable to display."))
    
    ggplot(coef_plot_df, aes(x = term, y = estimate, fill = term)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = round(estimate, 2)),
        vjust = -0.4,
        size = 3.5
      ) +
      labs(
        title = "Fixed-Effects Coefficient Estimates",
        x = "Variable",
        y = "Coefficient Estimate",
        fill = "Variable"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      )
  })
  
  output$panel_interpretation <- renderUI({
    mods <- panel_model()
    
    tbl <- mods$fe_tbl %>%
      filter(term %in% c("marriage", "divorce")) %>%
      mutate(term = recode(term, marriage = "Marriage", divorce = "Divorce"))
    
    if (!is.null(input$panel_vars) && length(input$panel_vars) > 0) {
      tbl <- tbl %>% filter(term %in% input$panel_vars)
    }
    
    validate(need(nrow(tbl) > 0, "Select at least one variable to display."))
    
    # Put Divorce first, then Marriage
    tbl <- tbl %>%
      mutate(term = factor(term, levels = c("Divorce", "Marriage"))) %>%
      arrange(term)
    
    lines <- apply(tbl, 1, function(row) {
      var <- row[["term"]]
      est <- as.numeric(row[["estimate"]])
      pval <- as.numeric(row[["p_value"]])
      
      direction <- ifelse(est > 0, "positive", "negative")
      strength <- ifelse(
        pval < 0.05,
        "statistically significant at the 5% level",
        "not statistically significant at the 5% level"
      )
      
      sprintf(
        "For <b>%s</b>, the fixed-effects coefficient is <b>%.3f</b>, indicating a <b>%s</b> same-year association with fertility. The p-value is <b>%.3f</b>, which means this result is <b>%s</b>.",
        var, est, direction, pval, strength
      )
    })
    
    HTML(sprintf(
      "<div class='note-box'><b>Result:</b> %s</div>",
      paste(lines, collapse = "<br><br>")
    ))
  })
}

shinyApp(ui, server)
