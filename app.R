# Install library yang dibutuhkan jika belum terinstal
# install.packages(c("shiny", "shinydashboard", "DT", "dplyr", "ggplot2", "rstatix"))

# Load library
library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rstatix)
library(plotly)

# Sample data
data <- data.frame(
  day = 1:10,
  left = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisis CTR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input_data"),
      menuItem("Analisis Statistik", tabName = "analisis_statistik"),
      menuItem("Visualisasi", tabName = "visualisasi")
    ),
    tags$style(
      HTML("
        .main-sidebar {
          background-color: red;
        }
        .main-sidebar a {
          color: white;
        }
      ")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #F0FFFF;
        }
        .content-wrapper, .right-side {
          background-color: #F0FFFF;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "input_data",
        h2("Input Data"),
        fluidRow(
          column(
            width = 6,
            textInput("left_input", "Masukkan data left", ""),
            textInput("center_input", "Masukkan data center", ""),
            textInput("right_input", "Masukkan data right", ""),
            actionButton("submit_btn", "Tambah Data")
          ),
          column(
            width = 6,
            DTOutput("data_table"),
            actionButton("edit_data_btn", "Edit Data Terpilih"),
            actionButton("hapus_data_btn", "Hapus Data Terpilih")
          ),
          column(
            width = 12,
            plotlyOutput("bar_plot")
          )
        )
      ),
      tabItem(
        tabName = "analisis_statistik",
        h2("Analisis Statistik"),
        verbatimTextOutput("output_anova"),
        box(
          title = "LEFT",
          solidHeader = TRUE,
          status = "primary",
          textOutput("box_left")
        ),
        box(
          title = "CENTER",
          solidHeader = TRUE,
          status = "warning",
          textOutput("box_center")
        ),
        box(
          title = "RIGHT",
          solidHeader = TRUE,
          status = "success",
          textOutput("box_right")
        )
      ),
      tabItem(
        tabName = "visualisasi",
        h2("Visualisasi"),
        plotlyOutput("bar_plot_visualisasi")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Inisialisasi data default
  rv <- reactiveValues(data = data, selected_rows = NULL, editing_row = NULL)
  
  # Menampilkan data tabel
  output$data_table <- renderDT({
    datatable(rv$data, editable = TRUE, selection = "multiple")
  })
  
  # Menambahkan data baru ke tabel
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left = as.numeric(input$left_input),
      center = as.numeric(input$center_input),
      right = as.numeric(input$right_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Mengatur baris yang dipilih
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })
  
  # Hapus data terpilih
  observeEvent(input$hapus_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })
  
  # Edit data terpilih
  observeEvent(input$edit_data_btn, {
    if (!is.null(rv$selected_rows) && length(rv$selected_rows) == 1) {
      rv$editing_row <- rv$selected_rows
      updateTextInput(session, "left_input", value = as.character(rv$data$left[rv$editing_row]))
      updateTextInput(session, "center_input", value = as.character(rv$data$center[rv$editing_row]))
      updateTextInput(session, "right_input", value = as.character(rv$data$right[rv$editing_row]))
    }
  })
  
  # Simpan perubahan setelah mengedit
  observeEvent(input$submit_btn, {
    if (!is.null(rv$editing_row)) {
      rv$data$left[rv$editing_row] <- as.numeric(input$left_input)
      rv$data$center[rv$editing_row] <- as.numeric(input$center_input)
      rv$data$right[rv$editing_row] <- as.numeric(input$right_input)
      rv$editing_row <- NULL
    }
  })
  
  # Analisis statistik
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  # Hindari analisis jika data kosong
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  
  # Menampilkan penjelasan hasil ANOVA
  output$box_left <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(left ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Karena p-value(", p_value, ") > 0.05, tidak ada perbedaan signifikan untuk kelompok left sidebar dan kelompok lainnya")
    } else {
      paste("Karena p-value(", p_value, ") < 0.05, ada perbedaan signifikan untuk kelompok left sidebar dan kelompok lainnya")
    }
  })
  
  output$box_center <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(center ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Karena p-value(", p_value, ") > 0.05, tidak ada perbedaan signifikan untuk kelompok center page dan kelompok lainnya")
    } else {
      paste("Karena p-value(", p_value, ") < 0.05, ada perbedaan signifikan untuk kelompok center page dan kelompok lainnya")
    }
  })
  
  output$box_right <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(right ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Karena p-value (", p_value, ")> 0.05, tidak ada perbedaan signifikan untuk kelompok right sidebar dan kelompok lainnya")
    } else {
      paste("Karena p-value(", p_value, ") < 0.05, ada perbedaan signifikan untuk kelompok right sidebar dan kelompok lainnya")
    }
  })
  # Visualisasi diagram batang
  output$bar_plot_visualisasi <- renderPlotly({
    if (is.null(rv$data)) return(NULL)
    
    # Data untuk plot
    data_plot <- rv$data %>%
      pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")
    
    # Warna untuk setiap kelompok
    colors <- c("cornsilk4", "lightcyan2", "pink")
    
    # Plot
    p <- plot_ly(data_plot, x = ~Group, y = ~Value, type = "bar", color = ~Group, colors = colors) %>%
      layout(title = "Sum of Click Based on Location",
             xaxis = list(title = "Location"),
             yaxis = list(title = "Sum"),
             showlegend = FALSE)
    
    return(p)
  })
  

}

# Run the application
shinyApp(ui, server)








