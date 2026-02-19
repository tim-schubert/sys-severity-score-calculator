library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(tidyverse)
library(ggplot2)
library(plotly)

# Constants
TAU <- 10
WHO_90P <- list(sit = 7.5, crawl = 10.5, walk_alone = 14.4)  # months (90th percentile)
SPEECH_REF <- list(first_word = 14, two_word_sentence = 24)  # months
SPEECH_DELAY_CUTOFF <- 12                                    # severe delay cutoff (months beyond ref)
SPEECH_MODERATE_CUTOFF <- 6                                 # moderate delay cutoff
AGE_GATE_MOTOR  <- max(unlist(WHO_90P))        # 14.4 mo
AGE_GATE_SPEECH <- max(unlist(SPEECH_REF))     # 24 mo

# Symptoms
symptom_rules <- tibble::tribble(
  ~Symptom, ~Weight, ~Group, ~B_i,
  "Motor delay", 2.0, "A", 1.5,
  "Speech delay", 2.0, "A", 1.5,
  "Intellectual Disability", 2.0, "B", 1.5,
  "Muscular hypotonia", 2.0, "Binary", 1.0,
  "Feeding difficulties (excl. hyperphagia)", 2.0, "Binary", 1.0,
  "Hyperphagia", 1.0, "Binary", 1.0,
  "Overweight", 1.0, "B", 1.5,
  "Underweight", 0.5, "Binary", 1.0,
  "Chronic constipation", 2.0, "Binary", 1.0,
  "Gastroesophageal reflux (GERD)", 2.0, "Binary", 1.0,
  "Sleep apnea", 2.0, "B", 1.5,
  "Respiratory distress", 1.0, "B", 1.5,
  "Contractures (and/or arthrogryposis)", 1.0, "Binary", 1.0,
  "Scoliosis or kyphosis", 1.0, "Binary", 1.0,
  "Seizures or epilepsy", 0.5, "Binary", 1.0,
  "Ocular issues", 1.0, "Binary", 1.0,
  "Genitourinary issues", 0.5, "Binary", 1.0,
  "GH deficiency", 1.0, "Binary", 1.0,
  "Endocrine issues", 0.5, "Binary", 1.0,
  "Autistic behavior", 1.0, "Binary", 1.0,
  "Anxiety", 1.0, "Binary", 1.0,
  "Other behavioral issues (except ASD and anxiety)", 1.0, "Binary", 1.0
)

symptom_levels <- symptom_rules$Symptom
symptom_palette <- setNames(
  viridisLite::viridis(length(symptom_levels), option = "D"),
  symptom_levels
)

# Helpers
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

compute_motor_bi <- function(ach_sit, sit_age, ach_crawl, crawl_age, ach_walk, walk_age, age_months) {
  if (is.na(age_months) || age_months < AGE_GATE_MOTOR) return(NA_real_)
  if (any(c(ach_sit, ach_crawl, ach_walk) == "no", na.rm = TRUE)) return(1.5)
  delayed <- c(
    if (identical(ach_sit,   "yes") && !is.na(sit_age))   sit_age   > WHO_90P$sit        else NA,
    if (identical(ach_crawl, "yes") && !is.na(crawl_age)) crawl_age > WHO_90P$crawl      else NA,
    if (identical(ach_walk,  "yes") && !is.na(walk_age))  walk_age  > WHO_90P$walk_alone else NA
  )
  if (all(is.na(delayed))) return(NA_real_)
  n <- sum(delayed, na.rm = TRUE)
  if (n >= 2) return(1.25)
  if (n == 1) return(1.0)
  0
}

compute_speech_bi <- function(ach_fw, fw_age, ach_2w, tw_age, age_months) {
  if (is.na(age_months) || age_months < AGE_GATE_SPEECH) return(NA_real_)
  if (any(c(ach_fw, ach_2w) == "no", na.rm = TRUE)) return(1.5)
  delayed_fw <- if (identical(ach_fw, "yes") && !is.na(fw_age)) {
    (fw_age - SPEECH_REF$first_word) > SPEECH_DELAY_CUTOFF
  } else NA
  delayed_2w <- if (identical(ach_2w, "yes") && !is.na(tw_age)) {
    (tw_age - SPEECH_REF$two_word_sentence) > SPEECH_DELAY_CUTOFF
  } else NA
  moderate_fw <- if (identical(ach_fw, "yes") && !is.na(fw_age)) {
    (fw_age - SPEECH_REF$first_word) > SPEECH_MODERATE_CUTOFF
  } else NA
  moderate_2w <- if (identical(ach_2w, "yes") && !is.na(tw_age)) {
    (tw_age - SPEECH_REF$two_word_sentence) > SPEECH_MODERATE_CUTOFF
  } else NA
  if (all(is.na(c(delayed_fw, delayed_2w)))) return(NA_real_)
  if (sum(c(delayed_fw, delayed_2w), na.rm = TRUE) >= 2) return(1.25)
  if (sum(c(moderate_fw, moderate_2w), na.rm = TRUE) >= 1) return(1.0)
  0
}

compute_id_bi <- function(status, iq) {
  if (status == "unknown") return(NA_real_)
  if (status == "absent")  return(0)
  if (!is.na(iq)) {
    if (iq < 30) return(1.5)
    if (iq < 50) return(1.25)
    return(1.0)  # iq >= 70 or 50 <= iq < 70
  }
  1.0
}

compute_overweight_bi <- function(status, bmi) {
  if (status == "unknown") return(NA_real_)
  if (status == "absent")  return(0)
  if (!is.na(bmi)) {
    if (bmi >= 35) return(1.5)
    if (bmi >= 30) return(1.0)
    if (bmi >= 25) return(0.5)
    return(0.5)
  }
  0.5
}

compute_sa_bi <- function(status, sev_choice) {
  if (status == "unknown") return(NA_real_)
  if (status == "absent")  return(0)
  if (nzchar(sev_choice)) {
    val <- suppressWarnings(as.numeric(sev_choice))
    if (!is.na(val)) return(val)
  }
  1.0
}

compute_rd_bi <- function(status, sev_choice) {
  if (status == "unknown") return(NA_real_)
  if (status == "absent")  return(0)
  if (nzchar(sev_choice)) {
    val <- suppressWarnings(as.numeric(sev_choice))
    if (!is.na(val)) return(val)
  }
  1.0
}

to_plotly_theme <- function(p, font_color = "#cfd4da", tooltip = "text", modebar = FALSE, legend_title = NULL,
                            x_title = NULL, y_title = NULL, show_x_ticks = TRUE, show_y_ticks = TRUE) {
  ggplotly(p, tooltip = tooltip) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = font_color),
      legend = list(
        orientation = "v",
        x = 1.02, xanchor = "left",
        y = 0.5,
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = font_color),
        title = list(text = legend_title %||% "", font = list(color = font_color))
      ),
      xaxis = list(
        title = x_title %||% "",
        titlefont=list(color=font_color),
        tickfont=list(color=font_color),
        showgrid=FALSE, zeroline=FALSE,
        showticklabels = show_x_ticks
      ),
      yaxis = list(
        title = y_title %||% "",
        titlefont=list(color=font_color),
        tickfont=list(color=font_color),
        showgrid=FALSE, zeroline=FALSE,
        showticklabels = show_y_ticks
      ),
      margin = list(l=50, r=10, t=10, b=40)
    ) |>
    plotly::config(displayModeBar = modebar) |>
    make_responsive_legend(breakpoint = 700)
}

sym_id <- function(name) gsub("[^a-z0-9]+", "_", tolower(name))
presence_status_choices <- c("Unknown" = "unknown", "Absent" = "absent", "Present" = "present")
milestone_status_choices <- c("Unknown" = "unknown", "Not yet" = "no", "Achieved" = "yes")
severity_level_choices <- c("Unknown" = "", "Present (severity unknown)" = "1.0", "Severe" = "1.5")

# Individual module
individualUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
    .dt-gray { color:#9aa0a6 !important; opacity:0.7; text-decoration: line-through; }
    .score-big { font-size:3rem; font-weight:900; margin-bottom:0.4rem; }
    .hint{ color:var(--sys-muted); }
    .milestone-col .shiny-input-container { margin-bottom:6px; }
    .shiny-options-group{ display:flex; flex-wrap:wrap; gap:12px; }
    .shiny-options-group .form-check-inline{ margin-right:0; margin-left:0; }
                       "))
    ),
    
    bslib::card(
      bslib::card_header("Individual"),
      fluidRow(
        column(6, textInput(ns("ind_id"), "ID (≤ 15 alphanumeric)", value = "")),
        column(3, numericInput(ns("age_years"), "Age (years)", value = NA, min = 0, step = 1)),
        column(3, numericInput(ns("age_months_only"), "Additional months", value = NA, min = 0, max = 11, step = 1))
      )
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Motor milestones"),
      uiOutput(ns("motor_ui"))
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Speech milestones"),
      uiOutput(ns("speech_ui"))
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Cognition & behavior"),
      radioButtons(ns("id_status"), "Intellectual disability (ID)",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      conditionalPanel(sprintf("input['%s']=='present'", ns("id_status")),
                       numericInput(ns("iq"), "IQ (optional)", value=NA, min=20, max=150, step=1)),
      radioButtons(ns(paste0("bin_", sym_id("Autistic behavior"))), "Autistic behavior",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Anxiety"))), "Anxiety",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Other behavioral issues (except ASD and anxiety)"))),
                   "Other behavioral issues (except ASD and anxiety)",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Seizures or epilepsy"))), "Seizures or epilepsy",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Body composition & appetite"),
      radioButtons(ns("ow_status"), "Overweight / obesity",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      conditionalPanel(sprintf("input['%s']=='present'", ns("ow_status")),
                       numericInput(ns("bmi"), "BMI (optional)", value=NA, min=5, max=80, step=0.1)),
      radioButtons(ns(paste0("bin_", sym_id("Underweight"))), "Underweight",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Hyperphagia"))), "Hyperphagia",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Endocrine"),
      radioButtons(ns(paste0("bin_", sym_id("GH deficiency"))), "GH deficiency",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Endocrine issues"))), "Endocrine issues",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Gastrointestinal & metabolic"),
      radioButtons(ns(paste0("bin_", sym_id("Feeding difficulties (excl. hyperphagia)"))),
                   "Feeding difficulties (excl. hyperphagia)",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Gastroesophageal reflux (GERD)"))),
                   "Gastroesophageal reflux (GERD)",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Chronic constipation"))), "Chronic constipation",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Respiratory & sleep"),
      radioButtons(ns("sa_status"), "Sleep apnea",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      conditionalPanel(sprintf("input['%s']=='present'", ns("sa_status")),
                       selectInput(ns("sa_sev"), "Severity (optional)", choices = severity_level_choices, selectize = FALSE)),
      radioButtons(ns("rd_status"), "Respiratory distress",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      conditionalPanel(sprintf("input['%s']=='present'", ns("rd_status")),
                       selectInput(ns("rd_sev"), "Severity (optional)", choices = severity_level_choices, selectize = FALSE))
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Musculoskeletal"),
      radioButtons(ns(paste0("bin_", sym_id("Muscular hypotonia"))), "Muscular hypotonia",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Contractures (and/or arthrogryposis)"))),
                   "Contractures (and/or arthrogryposis)",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Scoliosis or kyphosis"))), "Scoliosis or kyphosis",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    bslib::card(
      bslib::card_header("Other symptoms"),
      radioButtons(ns(paste0("bin_", sym_id("Ocular issues"))), "Ocular issues",
                   choices = presence_status_choices, inline=TRUE, selected="unknown"),
      radioButtons(ns(paste0("bin_", sym_id("Genitourinary issues"))), "Genitourinary issues",
                   choices = presence_status_choices, inline=TRUE, selected="unknown")
    ),
    br(),
    
    div(
      class = "compute-wrap",
      actionButton(ns("compute"), "Compute score", class = "btn btn-light btn-lg")
    ),
    br(), br(),
    
    conditionalPanel(
      condition = paste0("input['", ns("compute"), "'] > 0"),
      bslib::card(
        bslib::card_header("Per-item contributions"),
        DTOutput(ns("item_table"))
      ),
      br(),
      bslib::card(
        bslib::card_header("Summary"),
        uiOutput(ns("summary_ui")),
        br(),
        plotlyOutput(ns("sbar_always"), height = "420px")
      )
    )
  )
}

make_responsive_legend <- function(p, breakpoint = 700) {
  htmlwidgets::onRender(
    p,
    sprintf("
      function(el, x){
        function relayout(){
          var w = el.offsetWidth || (el.parentElement && el.parentElement.offsetWidth) || 1000;
          if (w < %d) {
            Plotly.relayout(el, {
              legend: {orientation: 'h', y: -0.35, x: 0.5, xanchor: 'center', bgcolor: 'rgba(0,0,0,0)'},
              margin: {l: 50, r: 10, t: 10, b: 110},
              'font.size': 12
            });
          } else {
            Plotly.relayout(el, {
              legend: {orientation: 'v', y: 0.5, x: 1.02, xanchor: 'left', bgcolor: 'rgba(0,0,0,0)'},
              margin: {l: 50, r: 10, t: 10, b: 40},
              'font.size': 14
            });
          }
        }
        relayout();
        window.addEventListener('resize', relayout);
      }", as.integer(breakpoint))
  )
}
individualServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    useShinyjs()
    
    sanitized_id <- reactive({
      raw <- input$ind_id %||% ""
      s <- gsub("[^A-Za-z0-9]", "", raw)
      substr(s, 1, 15)
    })
    observeEvent(input$ind_id, {
      s <- sanitized_id()
      if (!identical(s, input$ind_id)) updateTextInput(session, "ind_id", value = s)
    }, ignoreInit = TRUE)
    
    age_months <- reactive({
      to_num <- function(x) { if (is.null(x) || length(x) == 0) return(NA_real_); suppressWarnings(as.numeric(x)) }
      y <- to_num(input$age_years); m <- to_num(input$age_months_only)
      if (is.na(y) && is.na(m)) return(NA_real_)
      if (is.na(y)) y <- 0; if (is.na(m)) m <- 0
      y * 12 + m
    })
    
    output$motor_ui <- renderUI({
      am <- age_months()
      if (is.na(am)) {
        return(div(class = "hint",
                   "Please enter age to be able to assess developmental milestones."))
      }
      if (am < AGE_GATE_MOTOR) {
        return(div(class = "hint",
                   sprintf("Too young to assess motor milestones (scorable \u2265 %.1f months).",
                           AGE_GATE_MOTOR)))
      }
      fluidRow(
        column(
          4, class = "milestone-col",
          radioButtons(ns("ach_sit"), "Sitting without support achieved?",
                       choices = milestone_status_choices, inline=TRUE, selected="unknown"),
          conditionalPanel(sprintf("input['%s']=='yes'", ns("ach_sit")),
                           numericInput(ns("age_sit"), "Age at sitting (mo)", value=NA, min=0, step=0.1))
        ),
        column(
          4, class = "milestone-col",
          radioButtons(ns("ach_crawl"), "Crawling on hands/knees achieved?",
                       choices = milestone_status_choices, inline=TRUE, selected="unknown"),
          conditionalPanel(sprintf("input['%s']=='yes'", ns("ach_crawl")),
                           numericInput(ns("age_crawl"), "Age at crawling (mo)", value=NA, min=0, step=0.1))
        ),
        column(
          4, class = "milestone-col",
          radioButtons(ns("ach_walk"), "Walking alone achieved?",
                       choices = milestone_status_choices, inline=TRUE, selected="unknown"),
          conditionalPanel(sprintf("input['%s']=='yes'", ns("ach_walk")),
                           numericInput(ns("age_walk"), "Age at walking alone (mo)", value=NA, min=0, step=0.1))
        )
      )
    })
    
    output$speech_ui <- renderUI({
      am <- age_months()
      if (is.na(am)) {
        return(div(class = "hint",
                   "Please enter age to be able to assess developmental milestones."))
      }
      if (am < AGE_GATE_SPEECH) {
        return(div(class = "hint",
                   sprintf("Too young to assess speech milestones (scorable \u2265 %d months).",
                           AGE_GATE_SPEECH)))
      }
      fluidRow(
        column(
          6, class = "milestone-col",
          radioButtons(ns("ach_fw"), "First word achieved?",
                       choices = milestone_status_choices, inline=TRUE, selected="unknown"),
          conditionalPanel(sprintf("input['%s']=='yes'", ns("ach_fw")),
                           numericInput(ns("age_first_word"), "Age at first word (mo)", value=NA, min=0, step=0.1))
        ),
        column(
          6, class = "milestone-col",
          radioButtons(ns("ach_2w"), "Two-word sentence achieved?",
                       choices = milestone_status_choices, inline=TRUE, selected="unknown"),
          conditionalPanel(sprintf("input['%s']=='yes'", ns("ach_2w")),
                           numericInput(ns("age_two_word"), "Age at two-word sentence (mo)", value=NA, min=0, step=0.1))
        )
      )
    })
    
    
    compute_df <- reactive({
      req(input$compute)
      age_mo <- age_months()
      
      special <- c("Overweight","Sleep apnea","Respiratory distress")
      bins <- symptom_rules %>% filter(Group=="Binary" & !(Symptom %in% special))
      
      symptom_rules %>%
        mutate(
          b_i = pmap_dbl(list(Symptom, Group, row_number()), function(sym, grp, rowi) {
            if (sym == "Motor delay")
              return(compute_motor_bi(input$ach_sit, input$age_sit, input$ach_crawl, input$age_crawl,
                                      input$ach_walk, input$age_walk, age_mo))
            if (sym == "Speech delay")
              return(compute_speech_bi(input$ach_fw, input$age_first_word, input$ach_2w, input$age_two_word, age_mo))
            if (sym == "Intellectual Disability")
              return(compute_id_bi(input$id_status, input$iq))
            if (sym == "Overweight")
              return(compute_overweight_bi(input$ow_status, input$bmi))
            if (sym == "Sleep apnea")
              return(compute_sa_bi(input$sa_status, input$sa_sev))
            if (sym == "Respiratory distress")
              return(compute_rd_bi(input$rd_status, input$rd_sev))
            if (grp == "Binary") {
              status <- input[[ paste0("bin_", sym_id(sym)) ]] %||% "unknown"
              if (status == "unknown") return(NA_real_)
              if (status == "absent")  return(0)
              if (status == "present") return(1)
            }
            0
          })
        ) %>%
        mutate(`w_i*b_i` = Weight * b_i,
               Known = !is.na(b_i))
    })
    
    summary_vals <- reactive({
      df <- compute_df()
      sraw <- df %>% filter(!is.na(b_i)) %>% summarise(v = sum(`w_i*b_i`)) %>% pull(v)
      smax <- df %>% filter(!is.na(b_i)) %>% summarise(v = sum(Weight * B_i)) %>% pull(v)
      S <- if (!is.na(smax) && smax >= TAU && smax > 0) sraw / smax else NA_real_
      list(df = df, S_raw = sraw %||% 0, S_max = smax %||% 0, S = S, tau_ok = !is.na(S))
    })
    
    output$item_table <- renderDT({
      sv <- summary_vals()
      df <- sv$df %>% select(Symptom, Weight, B_i, b_i, `w_i*b_i`, Known)
      colnames(df) <- c("Symptom", "w", "B<sub>i</sub>", "b<sub>i</sub>", "w·b<sub>i</sub>", "Known")
      datatable(
        df, rownames = FALSE, escape = FALSE,
        options = list(
          paging = FALSE, searching = FALSE, info = FALSE,
          ordering = FALSE, dom = "t",
          columnDefs = list(list(visible = FALSE, targets = ncol(df)-1))
        ),
        callback = JS("
          table.rows().every(function(){
            var d=this.data(); var known = d[d.length-1];
            if(known===false){ $(this.node()).addClass('dt-gray'); }
          });
        ")
      )
    })
    
    output$sbar_always <- renderPlotly({
      sv <- summary_vals()
      df <- sv$df %>% filter(!is.na(b_i))
      if (nrow(df) == 0 || sv$S_max <= 0) return(NULL)
      
      plot_df <- bind_rows(
        df %>% transmute(Bar = "S_raw", Symptom, value = (`w_i*b_i`) / sv$S_max),
        df %>% transmute(Bar = "S_max", Symptom, value = (Weight * B_i) / sv$S_max)
      ) %>% mutate(value = pmin(pmax(value, 0), 1),
                   text = sprintf("%s<br>%s: %.1f%%", Symptom, Bar, 100*value),
                   Symptom = factor(Symptom, levels = rev(symptom_levels)),
                   Bar = factor(Bar, levels = c("S_raw", "S_max")))
      
      p <- ggplot(plot_df, aes(x = Bar, y = value, fill = Symptom, text = text)) +
        geom_bar(stat = "identity", width = 0.66) +
        coord_flip() +
        scale_fill_manual(values = symptom_palette, drop = FALSE) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
        theme_void(base_size = 12) +
        theme(legend.position = "right")
      theme_mode <- input$theme_mode %||% "dark"
      font_color <- if (identical(theme_mode, "light")) "#334155" else "#cfd4da"
      to_plotly_theme(
        p,
        font_color = font_color,
        tooltip = "text",
        legend_title = "Symptom",
        x_title = NULL,
        y_title = NULL,
        show_x_ticks = TRUE,
        show_y_ticks = TRUE
      )
    })
    
    output$summary_ui <- renderUI({
      sv <- summary_vals()
      if (!sv$tau_ok) {
        return(div(
          style = "text-align:center;",
          tagList(
            p(HTML(sprintf("<b>S<sub>raw</sub></b> = %.2f", sv$S_raw))),
            p(HTML(sprintf("<b>S<sub>max</sub></b> = %.2f", sv$S_max))),
            div(class = "alert alert-warning", HTML(sprintf(
              "Not enough information yet: <b>S<sub>max</sub>=%.2f</b> is below τ=10. Provide more details to compute S.", sv$S_max)))
          )
        ))
      }
      div(
        style = "text-align:center;",
        tagList(
          div(class = "score-big", HTML(sprintf("S = %.3f", sv$S))),
          tags$div(HTML(sprintf("<b>S<sub>raw</sub></b> = %.2f &nbsp;&nbsp; <b>S<sub>max</sub></b> = %.2f (τ=10)",
                                sv$S_raw, sv$S_max)))
        )
      )
    })
    
    binary_symptoms <- symptom_rules %>% filter(Group == "Binary") %>% pull(Symptom)
    binary_input_ids <- paste0("bin_", vapply(binary_symptoms, sym_id, character(1)))

    state_snapshot <- reactive({
      bin_vals <- setNames(
        lapply(binary_input_ids, function(id) input[[id]] %||% "unknown"),
        binary_input_ids
      )
      c(
        list(
          ind_id = input$ind_id %||% "",
          age_years = input$age_years,
          age_months_only = input$age_months_only,
          ach_sit = input$ach_sit %||% "unknown",
          age_sit = input$age_sit,
          ach_crawl = input$ach_crawl %||% "unknown",
          age_crawl = input$age_crawl,
          ach_walk = input$ach_walk %||% "unknown",
          age_walk = input$age_walk,
          ach_fw = input$ach_fw %||% "unknown",
          age_first_word = input$age_first_word,
          ach_2w = input$ach_2w %||% "unknown",
          age_two_word = input$age_two_word,
          id_status = input$id_status %||% "unknown",
          iq = input$iq,
          ow_status = input$ow_status %||% "unknown",
          bmi = input$bmi,
          sa_status = input$sa_status %||% "unknown",
          sa_sev = input$sa_sev %||% "",
          rd_status = input$rd_status %||% "unknown",
          rd_sev = input$rd_sev %||% "",
          computed = isTRUE((input$compute %||% 0) > 0)
        ),
        bin_vals
      )
    })

    restore_state <- function(st) {
      if (is.null(st) || !is.list(st)) return(invisible(NULL))
      update_num_if_known <- function(id, value) {
        if (!is.null(value) && length(value) > 0 && !is.na(value)) {
          updateNumericInput(session, id, value = value)
        }
      }
      updateTextInput(session, "ind_id", value = st$ind_id %||% "")
      update_num_if_known("age_years", st$age_years)
      update_num_if_known("age_months_only", st$age_months_only)
      updateRadioButtons(session, "ach_sit", selected = st$ach_sit %||% "unknown")
      update_num_if_known("age_sit", st$age_sit)
      updateRadioButtons(session, "ach_crawl", selected = st$ach_crawl %||% "unknown")
      update_num_if_known("age_crawl", st$age_crawl)
      updateRadioButtons(session, "ach_walk", selected = st$ach_walk %||% "unknown")
      update_num_if_known("age_walk", st$age_walk)
      updateRadioButtons(session, "ach_fw", selected = st$ach_fw %||% "unknown")
      update_num_if_known("age_first_word", st$age_first_word)
      updateRadioButtons(session, "ach_2w", selected = st$ach_2w %||% "unknown")
      update_num_if_known("age_two_word", st$age_two_word)
      updateRadioButtons(session, "id_status", selected = st$id_status %||% "unknown")
      update_num_if_known("iq", st$iq)
      updateRadioButtons(session, "ow_status", selected = st$ow_status %||% "unknown")
      update_num_if_known("bmi", st$bmi)
      updateRadioButtons(session, "sa_status", selected = st$sa_status %||% "unknown")
      updateSelectInput(session, "sa_sev", selected = st$sa_sev %||% "")
      updateRadioButtons(session, "rd_status", selected = st$rd_status %||% "unknown")
      updateSelectInput(session, "rd_sev", selected = st$rd_sev %||% "")
      for (id in binary_input_ids) {
        updateRadioButtons(session, id, selected = st[[id]] %||% "unknown")
      }
      if (isTRUE(st$computed)) {
        shinyjs::delay(120, {
          shinyjs::runjs(sprintf("
            var id = '%s';
            var el = document.getElementById(id);
            if (el && typeof el.click === 'function') {
              el.click();
            }
          ", ns("compute")))
        })
      }
      invisible(NULL)
    }

    list(
      get_summary = reactive(summary_vals()),
      get_id      = reactive(sanitized_id()),
      get_state   = reactive(state_snapshot()),
      restore_state = restore_state
    )
  })
}

# UI
ui <- tagList(
  fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),
    tags$head(
      tags$title("SYS Severity Score Calculator"),
      tags$style(HTML("
        :root{
          --bs-body-bg: #2b3138;
          --bs-body-color: #dfe4ea;
          --bs-border-color: rgba(255,255,255,0.10);
          --sys-header-line: #4dabf7;
          --sys-link: #4dabf7;
          --sys-link-hover: #74c0fc;
          --sys-muted: #adb5bd;
          --sys-card-bg: #202326;
          --sys-card-header-bg: #1b1e22;
          --sys-card-border: rgba(255,255,255,0.08);
          --sys-card-header-border: rgba(255,255,255,0.06);
          --sys-form-bg: #0d0d0d;
          --sys-form-border: #333;
          --sys-form-focus-border: #555;
          --sys-form-focus-shadow: rgba(85,85,85,0.25);
          --sys-select-hover-bg: #1a1a1a;
          --sys-btn-primary-bg: #f8f9fa;
          --sys-btn-primary-border: #f8f9fa;
          --sys-btn-primary-text: #1b1f23;
          --sys-btn-primary-hover-bg: #e9ecef;
          --sys-btn-primary-hover-border: #e9ecef;
          --sys-btn-primary-hover-text: #14181f;
          --sys-btn-outline-text: #dfe4ea;
          --sys-btn-outline-border: rgba(255,255,255,0.28);
          --sys-btn-outline-hover-bg: rgba(255,255,255,0.10);
          --sys-btn-outline-hover-text: #ffffff;
          --sys-modal-bg: #24272b;
          --sys-modal-border: rgba(255,255,255,0.14);
          --sys-bib-bg: #1f1f1f;
          --sys-bib-border: #2a2a2a;
        }
        html[data-sys-theme='light']{
          --bs-body-bg: #e1e7ef;
          --bs-body-color: #1f2937;
          --bs-border-color: #dbe2ea;
          --sys-header-line: #2563eb;
          --sys-link: #2563eb;
          --sys-link-hover: #1d4ed8;
          --sys-muted: #6b7280;
          --sys-card-bg: #ffffff;
          --sys-card-header-bg: #f7f9fc;
          --sys-card-border: rgba(17,24,39,0.10);
          --sys-card-header-border: rgba(17,24,39,0.08);
          --sys-form-bg: #ffffff;
          --sys-form-border: #cbd5e1;
          --sys-form-focus-border: #93c5fd;
          --sys-form-focus-shadow: rgba(37,99,235,0.22);
          --sys-select-hover-bg: #f1f5f9;
          --sys-btn-primary-bg: #2563eb;
          --sys-btn-primary-border: #2563eb;
          --sys-btn-primary-text: #ffffff;
          --sys-btn-primary-hover-bg: #1d4ed8;
          --sys-btn-primary-hover-border: #1d4ed8;
          --sys-btn-primary-hover-text: #ffffff;
          --sys-btn-outline-text: #1f2937;
          --sys-btn-outline-border: rgba(17,24,39,0.28);
          --sys-btn-outline-hover-bg: rgba(17,24,39,0.06);
          --sys-btn-outline-hover-text: #111827;
          --sys-modal-bg: #eef2f7;
          --sys-modal-border: rgba(17,24,39,0.18);
          --sys-bib-bg: #f8fafc;
          --sys-bib-border: #cbd5e1;
        }
        html, body, .container-fluid{
          background-color:var(--bs-body-bg) !important;
          color:var(--bs-body-color) !important;
        }
        .app-header{
          padding:14px 14px 16px 14px;
          text-align:center;
          border-bottom:none;
          margin-bottom:14px;
          position:relative;
        }
        .app-header::after{
          content:\"\";
          position:absolute;
          left:0;
          right:0;
          bottom:0;
          height:2px;
          background:var(--sys-header-line);
          opacity:0.7;
        }
        .app-header h1{
          font-family: inherit;
          font-weight: 600;
          font-size: 1.25rem;
          text-transform: uppercase;
          letter-spacing: .03em;
          margin: 0;
          padding: 0 56px;
          color: inherit;
        }
        @media (max-width: 768px){ .app-header h1{ font-size: 1rem; } }

        .page-container{max-width:1180px;margin:0 auto;padding:0 14px;}

        a{ color:var(--sys-link); text-decoration: none; }
        a:hover, a:focus{ color:var(--sys-link-hover); text-decoration: none; }
        .site-footer a{ color:var(--sys-muted) !important; text-decoration:none; }
        .site-footer a:hover, .site-footer a:focus{ color:var(--sys-link) !important; text-decoration:none; }

        .card{border:1px solid var(--sys-card-border)!important;border-radius:10px!important;background:var(--sys-card-bg);}
        .card-header{background:var(--sys-card-header-bg)!important;border-bottom:1px solid var(--sys-card-header-border)!important;color:var(--bs-body-color);padding:10px 14px!important;font-weight:600;}
        .card-body{padding:14px!important;}
        .card-footer{background:var(--sys-card-bg);border-top:1px solid var(--sys-card-header-border);padding:12px 14px;}

        .sidebar{position:relative;top:auto;max-height:none;overflow:visible;}
        .well.sidebar{
          background:transparent !important;
          border:none !important;
          box-shadow:none !important;
          padding-top:0 !important;
        }
        .sidebar .btn{width:100%;margin-bottom:8px;}
        .sidebar .sidebar-download{margin-bottom:14px !important;}
        .action-row{display:flex;gap:8px;}

        .muted{color:var(--sys-muted);}
        h4{font-size:1.05rem;font-weight:600;color:var(--bs-body-color);margin-top:0;}

        .toolbar-bar{display:flex;flex-wrap:wrap;align-items:center;gap:8px;margin-bottom:14px;padding:0;background:transparent;border:none;}
        .toolbar-bar .toolbar-spacer{flex:1;min-width:8px;}
        .toolbar-bar .form-group{margin-bottom:0;flex-shrink:0;min-width:28ch;}
        .toolbar-bar select{min-width:100%;max-width:100%;padding-right:2.25em;box-sizing:border-box;background:var(--sys-form-bg) !important;border-color:var(--sys-form-border) !important;color:var(--bs-body-color) !important;}
        .toolbar-bar select:focus{background:var(--sys-form-bg) !important;border-color:var(--sys-form-focus-border) !important;color:var(--bs-body-color) !important;}
        .toolbar-bar select option{background:var(--sys-form-bg) !important;color:var(--bs-body-color) !important;}
        .selectize-dropdown{background:var(--sys-form-bg) !important;border:1px solid var(--sys-form-border) !important;}
        .selectize-dropdown .option,.selectize-dropdown .optgroup-header{background:var(--sys-form-bg) !important;color:var(--bs-body-color) !important;}
        .selectize-dropdown .option.active,.selectize-dropdown .option:hover{background:var(--sys-select-hover-bg) !important;color:var(--bs-body-color) !important;}
        .selectize-input{background:var(--sys-form-bg) !important;border-color:var(--sys-form-border) !important;color:var(--bs-body-color) !important;}
        .selectize-input input{color:var(--bs-body-color) !important;}
        .selectize-input.focus{border-color:var(--sys-form-focus-border) !important;box-shadow:0 0 0 0.2rem var(--sys-form-focus-shadow);}
        .toolbar-bar .btn{margin:0;}
        #theme_toggle{
          position:absolute;
          right:14px;
          top:50%;
          transform:translateY(-50%);
          padding:2px 4px;
          min-width:auto;
          border:none !important;
          background:transparent !important;
          box-shadow:none !important;
          font-size:0;
          line-height:1;
          color:var(--sys-btn-outline-text) !important;
          opacity:0.9;
          text-decoration:none !important;
        }
        #theme_toggle::before{
          content:'✶';
          font-size:16px;
          line-height:1;
          color:currentColor;
        }
        html[data-sys-theme='light'] #theme_toggle::before{ content:'☾'; }
        #theme_toggle:hover, #theme_toggle:focus{
          opacity:1;
          color:var(--sys-btn-outline-hover-text) !important;
          text-decoration:none !important;
          border:none !important;
          background:transparent !important;
          box-shadow:none !important;
        }
        #theme_toggle:active{border:none !important;background:transparent !important;box-shadow:none !important;text-decoration:none !important;}
        #theme_toggle:focus-visible{
          outline:1px solid var(--sys-btn-outline-border);
          outline-offset:2px;
        }
        #add_ind.btn-primary{
          background-color:var(--sys-btn-primary-bg) !important;
          border-color:var(--sys-btn-primary-border) !important;
          color:var(--sys-btn-primary-text) !important;
          font-weight:600;
        }
        #add_ind.btn-primary:hover,
        #add_ind.btn-primary:focus{
          background-color:var(--sys-btn-primary-hover-bg) !important;
          border-color:var(--sys-btn-primary-hover-border) !important;
          color:var(--sys-btn-primary-hover-text) !important;
        }
        #add_ind.btn-primary:focus{
          box-shadow:0 0 0 0.2rem var(--sys-form-focus-shadow) !important;
        }
        .compute-wrap{width:100%;}
        .compute-wrap > .shiny-input-container{width:100%;margin-bottom:0;}
        button[id$='-compute']{
          background-color:var(--sys-btn-primary-bg) !important;
          border-color:var(--sys-btn-primary-border) !important;
          color:var(--sys-btn-primary-text) !important;
          font-weight:700;
          width:100%;
          display:block;
        }
        button[id$='-compute']:hover,
        button[id$='-compute']:focus{
          background-color:var(--sys-btn-primary-hover-bg) !important;
          border-color:var(--sys-btn-primary-hover-border) !important;
          color:var(--sys-btn-primary-hover-text) !important;
        }
        button[id$='-compute']:focus{
          box-shadow:0 0 0 0.2rem var(--sys-form-focus-shadow) !important;
        }
        .main-view{transition:opacity 120ms ease-in-out;}

        .form-control, .form-select, input[type=\"text\"], input[type=\"number\"], select, textarea{
          background-color:var(--sys-form-bg) !important;
          border-color:var(--sys-form-border) !important;
          color:var(--bs-body-color) !important;
        }
        .form-control:focus, .form-select:focus, input:focus, select:focus, textarea:focus{
          background-color:var(--sys-form-bg) !important;
          border-color:var(--sys-form-focus-border) !important;
          color:var(--bs-body-color) !important;
          box-shadow:0 0 0 0.2rem var(--sys-form-focus-shadow);
        }
        .form-control::placeholder{color:#6c757d;}
        input[type=\"number\"]{color-scheme:dark;}
        html[data-sys-theme='light'] input[type=\"number\"]{color-scheme:light;}
        select.form-control option, .form-select option{background:var(--sys-form-bg);color:var(--bs-body-color);}
        .shiny-options-group .form-check-inline{
          display:inline-flex;
          align-items:center;
          margin-right:1rem;
          margin-left:0 !important;
          padding-left:0;
          line-height:1.2;
        }
        .shiny-options-group .form-check-inline .form-check-input{
          float:none;
          position:relative;
          margin:0 .4rem 0 0 !important;
          flex:0 0 auto;
          top:0;
        }
        .shiny-options-group .form-check-inline .form-check-label{
          display:inline-flex;
          align-items:center;
          line-height:1.2;
          margin:0;
        }
        .shiny-options-group input[type='radio']{
          appearance:auto !important;
          -webkit-appearance:radio !important;
          accent-color:var(--sys-link);
          width:1rem;
          height:1rem;
          margin:0 .4rem 0 0 !important;
          vertical-align:middle;
          opacity:1 !important;
          background:transparent !important;
          box-shadow:none !important;
          border:none !important;
        }
        html[data-sys-theme='light'] .shiny-options-group input[type='radio']{
          accent-color:#1d4ed8;
        }
        .form-check-input{background-color:var(--sys-form-bg) !important;border-color:var(--sys-form-focus-border) !important;}
        .form-check-input[type='radio']{border-width:2px;}
        .form-check-input:checked{background-color:var(--sys-link) !important;border-color:var(--sys-link) !important;}
        .form-check-input[type='radio']:checked{
          background-image:none !important;
          box-shadow:none !important;
        }
        html[data-sys-theme='light'] .form-check-input[type='radio']{
          border-color:#64748b !important;
        }
        html[data-sys-theme='light'] .form-check-input[type='radio']:not(:checked){
          background-color:#e2e8f0 !important;
          border-color:#64748b !important;
        }
        html[data-sys-theme='light'] .form-check-input[type='radio']:checked{
          border-color:#1d4ed8 !important;
          background-color:#1d4ed8 !important;
          box-shadow:none !important;
        }
        .form-check-input:focus{border-color:var(--sys-form-focus-border);box-shadow:0 0 0 0.2rem var(--sys-form-focus-shadow);}
        .form-check:has(.form-check-input:checked) .form-check-label{
          font-weight:600;
        }
        html[data-sys-theme='light'] .form-check:has(.form-check-input:checked) .form-check-label{
          color:#1d4ed8;
        }

        .btn-dark{
          background-color:var(--sys-card-header-bg) !important;
          border-color:var(--sys-card-header-border) !important;
          color:var(--bs-body-color) !important;
        }
        .btn-dark:hover, .btn-dark:focus{
          filter:brightness(1.08);
          color:var(--bs-body-color) !important;
        }

        .site-footer__inner{max-width:1180px;margin:0 auto;padding:20px 14px;display:flex;align-items:center;justify-content:space-between;gap:14px;}
        .site-footer__logo{height:60px;flex:0 0 auto;}
        html[data-sys-theme='light'] .site-footer__logo{filter:invert(1);}
        .site-footer__center{flex:1 1 auto;text-align:center;color:var(--sys-muted);}
        .site-footer{position:relative;z-index:10;}
        @media (max-width:768px){.site-footer__logo{height:42px;}.site-footer__center{font-size:0.88rem;}}

        .modal-content{background:var(--sys-modal-bg);border:1px solid var(--sys-modal-border);color:var(--bs-body-color);}
        .modal-header,.modal-footer{border-color:var(--sys-card-header-border);}

        .bib-link{cursor:pointer;text-decoration:underline;color:var(--sys-link);font-size:inherit;}
        .bib-link:hover,.bib-link:focus{color:var(--sys-link-hover);text-decoration:underline;}
        .aff-toggle{
          cursor:pointer;
          color:var(--sys-muted);
          text-decoration:none;
          font-size:0.86rem;
          display:inline-block;
          margin-top:8px;
        }
        .aff-toggle:hover,.aff-toggle:focus{
          color:var(--bs-body-color);
          text-decoration:none;
        }
        .affiliations-panel{
          margin-top:6px;
          padding-top:6px;
          border-top:1px solid var(--sys-card-header-border);
          font-size:0.86rem;
          line-height:1.35;
          color:var(--sys-muted);
        }
        #bibtex_block{white-space:pre-wrap;font-size:.86rem;background:var(--sys-bib-bg);padding:10px;border-radius:8px;border:1px solid var(--sys-bib-border);color:var(--bs-body-color);}

        /* Enforce high-contrast chart text in light mode */
        html[data-sys-theme='light'] .js-plotly-plot .xtick text,
        html[data-sys-theme='light'] .js-plotly-plot .ytick text,
        html[data-sys-theme='light'] .js-plotly-plot .legendtext,
        html[data-sys-theme='light'] .js-plotly-plot .legendtitletext,
        html[data-sys-theme='light'] .js-plotly-plot .gtitle,
        html[data-sys-theme='light'] .js-plotly-plot .annotation-text{
          fill:#334155 !important;
          color:#334155 !important;
        }
      ")),
      tags$script(HTML("
        (function(){
          var root = document.documentElement;
          var STORAGE_KEY = 'sys-theme-override';
          var LEGACY_KEY = 'sys-theme';
          var media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;

          function getSystemTheme(){
            return media && media.matches ? 'dark' : 'light';
          }
          function getStoredOverride(){
            var stored = null;
            try {
              stored = localStorage.getItem(STORAGE_KEY);
              if (stored !== 'dark' && stored !== 'light') {
                var legacy = localStorage.getItem(LEGACY_KEY);
                if (legacy === 'dark' || legacy === 'light') {
                  stored = legacy;
                  localStorage.setItem(STORAGE_KEY, legacy);
                } else {
                  stored = null;
                }
              }
            } catch(e) {}
            return stored;
          }
          function getPreferredTheme(){
            var override = getStoredOverride();
            return override || getSystemTheme();
          }
          function hasManualOverride(){
            return getStoredOverride() !== null;
          }
          function setManualOverride(mode){
            try { localStorage.setItem(STORAGE_KEY, mode); } catch(e) {}
          }
          function applyTheme(mode){
            root.setAttribute('data-sys-theme', mode);
            if (window.Shiny && window.Shiny.setInputValue) {
              window.Shiny.setInputValue('theme_mode', mode, {priority:'event'});
            }
            var btn = document.getElementById('theme_toggle');
            if (btn) {
              var next = mode === 'light' ? 'dark' : 'light';
              var label = next === 'light' ? 'Switch to light mode' : 'Switch to dark mode';
              btn.setAttribute('title', label);
              btn.setAttribute('aria-label', label);
            }
          }
          function syncToSystemTheme(){
            if (hasManualOverride()) return;
            applyTheme(getSystemTheme());
          }
          document.addEventListener('DOMContentLoaded', function(){
            applyTheme(getPreferredTheme());
            if (media) {
              if (typeof media.addEventListener === 'function') {
                media.addEventListener('change', syncToSystemTheme);
              } else if (typeof media.addListener === 'function') {
                media.addListener(syncToSystemTheme);
              }
            }
          });
          document.addEventListener('shiny:connected', function(){
            applyTheme(getPreferredTheme());
          });
          document.addEventListener('click', function(e){
            var toggle = e.target.closest && e.target.closest('#theme_toggle');
            if (toggle) {
              var current = root.getAttribute('data-sys-theme') === 'light' ? 'light' : 'dark';
              var next = current === 'light' ? 'dark' : 'light';
              setManualOverride(next);
              applyTheme(next);
              return;
            }
            var aff = e.target.closest && e.target.closest('#toggle_affiliations');
            if (aff) {
              e.preventDefault();
              var panel = document.getElementById('affiliations_panel');
              if (!panel) return;
              var expanded = aff.getAttribute('data-expanded') === 'true';
              var next = !expanded;
              panel.style.display = next ? 'block' : 'none';
              aff.setAttribute('data-expanded', next ? 'true' : 'false');
              aff.textContent = next ? 'Hide affiliations' : 'Show affiliations';
              return;
            }
            var btn = e.target.closest && e.target.closest('[data-copy-target]');
            if (!btn) return;
            var sel = btn.getAttribute('data-copy-target');
            var el = document.querySelector(sel);
            if (!el) return;
            navigator.clipboard.writeText(el.innerText).then(function(){
              var old = btn.textContent;
              btn.textContent = 'Copied!';
              setTimeout(function(){ btn.textContent = old; }, 1200);
            });
          });
        })();
      "))
    ),
    tags$header(
      class = "app-header",
      tags$h1("Schaaf-Yang Syndrome Severity Score Calculator"),
      actionButton("theme_toggle", "Toggle theme", class = "btn btn-link")
    ),
    div(class = "page-container",
        sidebarLayout(
          sidebarPanel(
            width = 3, class = "sidebar",
            downloadButton("dl_summary", "Download summary", class = "btn btn-dark sidebar-download"),
            bslib::card(
              bslib::card_header("Reference"),
              bslib::card_body(
                tags$div(
                  class = "muted",
                  style = "font-size:0.9em; margin-bottom:0;",
                  tags$span("Schubert et al. (2025) · "),
                  actionLink("show_bib", "BibTeX", class = "bib-link")
                )
              )
            ),
            bslib::card(
              bslib::card_header("Important Notes"),
              bslib::card_body(
                tags$p(class = "muted", style = "font-size:0.9em; line-height:1.35; margin-bottom:10px;",
                       tags$b("Data protection:"), " You are solely responsible for compliance with privacy and data-protection laws and policies."),
                tags$p(class = "muted", style = "font-size:0.9em; line-height:1.35; margin-bottom:10px;",
                       tags$b("Intent:"), " The score is not meant to reduce people to numbers. It is intended as a tool to help advance knowledge for individuals with SYS."),
                tags$p(class = "muted", style = "font-size:0.9em; line-height:1.35; margin-bottom:10px;",
                       tags$b("Purpose:"), " This score and app are solely designed for research purposes."),
                tags$p(class = "muted", style = "font-size:0.9em; line-height:1.35; margin-bottom:10px;",
                       "We are deeply grateful to individuals with SYS and their families for their time, trust, and contributions.")
              )
            ),
            bslib::card(
              bslib::card_header("Contributors"),
              bslib::card_body(
                tags$div(
                  class = "muted",
                  style = "font-size:0.9em; line-height:1.4;",
                  tags$div(
                    "Tim Schubert",
                    tags$sup("1")
                  ),
                  tags$div(
                    "Antonia Tietzel",
                    tags$sup("1,2")
                  ),
                  tags$div("Hari Pottayil", tags$sup("1")),
                  tags$div("Pilar Caro", tags$sup("1")),
                  tags$div("Rachel B. Gilmore", tags$sup("1")),
                  tags$div("Felix Franke", tags$sup("1")),
                  tags$div("Ferdinand Althammer", tags$sup("1")),
                  tags$div("Christian P. Schaaf", tags$sup("1")),
                  tags$a(
                    id = "toggle_affiliations",
                    href = "#",
                    class = "aff-toggle",
                    `data-expanded` = "false",
                    "Show affiliations"
                  ),
                  tags$div(
                    id = "affiliations_panel",
                    class = "affiliations-panel",
                    style = "display:none;",
                    tags$div(tags$sup("1"), " Institute of Human Genetics, Heidelberg University, Heidelberg, Germany"),
                    tags$div(tags$sup("2"), " Clinical Cooperation Unit Neuropathology, German Cancer Research Center (DKFZ), Heidelberg, Germany")
                  )
                )
              )
            ),
            bslib::card(
              bslib::card_header("Software"),
              bslib::card_body(
                tags$div(
                  class = "muted",
                  style = "font-size:0.9em; line-height:1.4;",
                  tags$p(
                    style = "margin-bottom:10px;",
                    "R version 4.4.2"
                  ),
                  tags$p(
                    style = "margin-bottom:0;",
                    "Shiny package (version 1.11.1)"
                  )
                )
              )
            ),
            bslib::card(
              bslib::card_header("IT Infrastructure"),
              bslib::card_body(
                tags$div(
                  class = "muted",
                  style = "font-size:0.9em; line-height:1.4;",
                  tags$p(
                    style = "margin-bottom:10px;",
                    "de.NBI Cloud"
                  ),
                  tags$p(
                    style = "margin-bottom:0;",
                    "University Computing Centre Heidelberg"
                  )
                )
              )
            )
          ),
          mainPanel(
            width = 9,
            uiOutput("toolbar"),
            uiOutput("main_content")
          )
        )
    )
  ),
  tags$footer(
    class = "site-footer",
    style = "background:transparent; color:var(--bs-body-color);",
    tags$div(
      class = "site-footer__inner",
      tags$img(src = "https://lh3.googleusercontent.com/d/12vCwvlmN8xrKnlU1GsDGJeDBgq_yl0sM", class = "site-footer__logo"),
      tags$div(
        class = "site-footer__center",
        tags$p(style = "margin:0;",
               tags$span(HTML("&copy; 2026 ")),
               tags$a(href = "https://tim-schubert.github.io", target = "_blank", rel = "noopener noreferrer", "Tim Schubert"),
               tags$span(HTML(" &middot; ")),
               tags$a(href = "https://www.apache.org/licenses/LICENSE-2.0.html", target = "_blank", "Apache License 2.0"),
               tags$span(HTML(" &middot; ")),
               tags$a(
                 href = "#",
                 "Legal notice (Impressum)",
                 onclick = "Shiny.setInputValue('open_legal', Date.now(), {priority:'event'}); return false;"
               )
        )
      ),
      tags$img(src = "https://lh3.googleusercontent.com/d/1BPl641wAs67xCTAKEEETVO0zjzOYUrda", class = "site-footer__logo")
    )
  )
)

# Server
server <- function(input, output, session) {
  counter <- reactiveVal(0)
  mods <- reactiveValues()
  active_ids <- reactiveVal(character(0))
  selected <- reactiveVal(NA_character_)
  state_cache <- reactiveValues()
  last_selected <- reactiveVal(NULL)

  add_individual <- function() {
    i_prev <- isolate(counter())
    i <- i_prev + 1
    counter(i)
    internal_id <- paste0("ind_", i)
    m <- individualServer(internal_id)
    mods[[internal_id]] <- m
    active_ids(c(isolate(active_ids()), internal_id))
    internal_id
  }
  
  bibtex_content <- "@article{SYSScorePaper,
  author  = {Schubert, T. and Tietzel, A. and Pottayil, H. and Caro, P. and Gilmore, R. B. and Franke, F. and Althammer, F. and Schaaf, C.P.},
  title   = {A blueprint for protein-centric genotype-phenotype investigations in single-exon disease genes applied to MAGEL2 and Schaaf-Yang syndrome},
  journal = {Journal},
  year    = {2025},
  volume  = {VOLUME},
  number  = {ISSUE},
  pages   = {PAGES},
  doi     = {10.xxxx/xxxxx}
}"
  observeEvent(input$show_bib, {
    showModal(modalDialog(
      tags$pre(id = "bibtex_block", bibtex_content),
      title = "BibTeX",
      easyClose = TRUE,
      size = "m",
      footer = tagList(
        tags$button(
          type = "button",
          class = "btn btn-primary",
          `data-copy-target` = "#bibtex_block",
          "Copy to clipboard"
        ),
        modalButton("Close")
      )
    ))
  })
  observeEvent(input$open_legal, {
    showModal(modalDialog(
      title = "Legal notice (Impressum)",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      tags$div(
        style = "line-height:1.45;",
        tags$h5("Angaben gemäß § 5 TMG", style = "margin-top:0;"),
        tags$p(
          "Institut für Humangenetik", tags$br(),
          "Tim Schubert", tags$br(),
          "Im Neuenheimer Feld 366, 5. OG", tags$br(),
          "69120 Heidelberg", tags$br(),
          "Germany"
        ),
        tags$h5("Kontakt"),
        tags$p("E-Mail: tim dot schubert at med dot uni-heidelberg dot de"),
        tags$h5("Verantwortlich für den Inhalt nach § 55 Abs. 2 RStV"),
        tags$p(
          "Institut für Humangenetik", tags$br(),
          "Tim Schubert", tags$br(),
          "Im Neuenheimer Feld 366, 5. OG", tags$br(),
          "69120 Heidelberg", tags$br(),
          "Germany"
        ),
        tags$h5("Verbraucherstreitbeilegung / Universalschlichtungsstelle"),
        tags$p(
          class = "muted",
          "Wir sind nicht bereit oder verpflichtet, an Streitbeilegungsverfahren vor einer Verbraucherschlichtungsstelle teilzunehmen."
        ),
        tags$h5("Haftung für Inhalte"),
        tags$p(
          class = "muted",
          "Als Diensteanbieter sind wir gemäß § 7 Abs.1 TMG für eigene Inhalte auf diesen Seiten nach den allgemeinen Gesetzen verantwortlich. Nach §§ 8 bis 10 TMG sind wir als Diensteanbieter jedoch nicht verpflichtet, übermittelte oder gespeicherte fremde Informationen zu überwachen oder nach Umständen zu forschen, die auf eine rechtswidrige Tätigkeit hinweisen.

Verpflichtungen zur Entfernung oder Sperrung der Nutzung von Informationen nach den allgemeinen Gesetzen bleiben hiervon unberührt. Eine diesbezügliche Haftung ist jedoch erst ab dem Zeitpunkt der Kenntnis einer konkreten Rechtsverletzung möglich. Bei Bekanntwerden von entsprechenden Rechtsverletzungen werden wir diese Inhalte umgehend entfernen."
        ),
        tags$h5("Haftung für Links"),
        tags$p(
          class = "muted",
          "Unser Angebot enthält Links zu externen Websites Dritter, auf deren Inhalte wir keinen Einfluss haben. Deshalb können wir für diese fremden Inhalte auch keine Gewähr übernehmen. Für die Inhalte der verlinkten Seiten ist stets der jeweilige Anbieter oder Betreiber der Seiten verantwortlich. Die verlinkten Seiten wurden zum Zeitpunkt der Verlinkung auf mögliche Rechtsverstöße überprüft. Rechtswidrige Inhalte waren zum Zeitpunkt der Verlinkung nicht erkennbar.

Eine permanente inhaltliche Kontrolle der verlinkten Seiten ist jedoch ohne konkrete Anhaltspunkte einer Rechtsverletzung nicht zumutbar. Bei Bekanntwerden von Rechtsverletzungen werden wir derartige Links umgehend entfernen."
        ),
        tags$h5("Urheberrecht"),
        tags$p(
          class = "muted",
          "Die durch die Seitenbetreiber erstellten Inhalte und Werke auf diesen Seiten unterliegen dem deutschen Urheberrecht. Die Vervielfältigung, Bearbeitung, Verbreitung und jede Art der Verwertung außerhalb der Grenzen des Urheberrechtes bedürfen der schriftlichen Zustimmung des jeweiligen Autors bzw. Erstellers. Downloads und Kopien dieser Seite sind nur für den privaten, nicht kommerziellen Gebrauch gestattet."
        ),
        tags$p(
          class = "muted",
          "Soweit die Inhalte auf dieser Seite nicht vom Betreiber erstellt wurden, werden die Urheberrechte Dritter beachtet. Insbesondere werden Inhalte Dritter als solche gekennzeichnet. Sollten Sie trotzdem auf eine Urheberrechtsverletzung aufmerksam werden, bitten wir um einen entsprechenden Hinweis. Bei Bekanntwerden von Rechtsverletzungen werden wir derartige Inhalte umgehend entfernen. Quelle: e-recht24.de"
        )
      )
    ))
  })
  
  dropdown_entries <- reactive({
    ids <- active_ids()
    if (!length(ids)) return(list())
    lapply(seq_along(ids), function(i) {
      k <- ids[[i]]
      m <- mods[[k]]
      if (is.null(m)) return(NULL)
      num <- i
      cached_label <- tryCatch({
        st <- state_cache[[k]]
        if (is.list(st) && nzchar(st$ind_id %||% "")) st$ind_id else ""
      }, error = function(e) "")
      lab <- if (nzchar(cached_label)) {
        cached_label
      } else {
        tryCatch(
          if (nzchar(m$get_id())) {
            m$get_id()
          } else if (!is.na(num)) {
            sprintf("Individual %d (enter ID in form)", num)
          } else {
            "Enter ID in form"
          },
          error = function(e) "Enter ID in form"
        )
      }
      S_val <- tryCatch({ s <- m$get_summary(); if (!is.null(s$S) && !is.na(s$S)) round(s$S, 2) else NA_real_ }, error = function(e) NA_real_)
      list(id = k, label = if (!is.na(S_val)) sprintf("%s (S = %.2f)", lab, S_val) else lab)
    }) %>% purrr::compact()
  })
  
  output$toolbar <- renderUI({
    entries <- dropdown_entries()
    cur <- selected()
    ind_choices <- if (length(entries)) setNames(vapply(entries, function(e) e$id, character(1)), vapply(entries, function(e) e$label, character(1))) else character(0)
    choices <- ind_choices
    valid_ids <- vapply(entries, function(e) e$id, character(1))
    sel <- if (cur %in% valid_ids) cur else if (length(valid_ids)) valid_ids[1] else ""
    div(class = "toolbar-bar",
      actionButton("add_ind", "+ Add", class = "btn btn-primary"),
      actionButton("rm_all", "Remove all", class = "btn btn-dark"),
      div(class = "toolbar-spacer", style = "flex:1; min-width:8px;"),
      selectizeInput(
        "individual_select",
        label = NULL,
        choices = choices,
        selected = sel,
        width = "auto",
        options = list(create = TRUE, placeholder = "Select or type new ID...")
      )
    )
  })
  
  observeEvent(input$individual_select, {
    val <- input$individual_select
    if (is.null(val) || !nzchar(trimws(val))) {
      return()
    }
    val <- trimws(val)
    entries <- dropdown_entries()
    valid_ids <- vapply(entries, function(e) e$id, character(1))
    if (val %in% valid_ids) {
      selected(val)
      return()
    }
    for (e in entries) {
      if (identical(e$label, val)) {
        selected(e$id)
        updateSelectInput(session, "individual_select", selected = e$id)
        return()
      }
    }
    new_id <- add_individual()
    id_sanitized <- substr(gsub("[^A-Za-z0-9]", "", val), 1, 15)
    if (nzchar(id_sanitized)) {
      st <- isolate(state_cache[[new_id]])
      if (is.null(st) || !is.list(st)) st <- list()
      st$ind_id <- id_sanitized
      state_cache[[new_id]] <- st
      updateTextInput(session, iid(new_id, "ind_id"), value = id_sanitized)
    }
    selected(new_id)
    updateSelectInput(session, "individual_select", selected = new_id)
  }, ignoreInit = TRUE)
  
  output$main_content <- renderUI({
    ids <- active_ids()
    if (!length(ids)) {
      return(bslib::card(
        bslib::card_header("Cohort"),
        bslib::card_body(
          tags$p(class = "muted", "No individuals in the cohort."),
          actionButton("add_ind_empty", "+ Add individual", class = "btn btn-primary")
        )
      ))
    }
    cur <- selected()
    cur <- if (!is.na(cur) && nzchar(cur) && cur %in% ids) cur else ids[1]
    tagList(
      lapply(ids, function(id) {
        div(
          id = paste0("ind-view-", id),
          class = "main-view",
          style = if (identical(id, cur)) "display:block; opacity:1;" else "display:none; opacity:0;",
          individualUI(id)
        )
      })
    )
  })

  observe({
    ids <- active_ids()
    if (!length(ids)) return()
    cur <- selected()
    target_id <- if (!is.na(cur) && nzchar(cur) && cur %in% ids) cur else ids[1]
    target <- paste0("#ind-view-", target_id)
    shinyjs::delay(20, {
      shinyjs::runjs(sprintf("
        (function(){
          var views = document.querySelectorAll('.main-view');
          for (var i = 0; i < views.length; i++) {
            views[i].style.opacity = '0';
            views[i].style.display = 'none';
          }
          var el = document.querySelector('%s');
          if (el) {
            el.style.display = 'block';
            requestAnimationFrame(function(){ el.style.opacity = '1'; });
          }
        })();
      ", target))
    })
  })

  observeEvent(selected(), {
    cur <- selected()
    prev <- isolate(last_selected())

    if (!is.null(prev) && !is.na(prev) && nzchar(prev) && !identical(prev, cur)) {
      mod_prev <- mods[[prev]]
      if (!is.null(mod_prev)) {
        prev_state <- try(mod_prev$get_state(), silent = TRUE)
        if (!inherits(prev_state, "try-error")) {
          state_cache[[prev]] <- prev_state
        }
      }
    }

    last_selected(cur)

    if (!is.na(cur) && nzchar(cur)) {
      mod_cur <- mods[[cur]]
      cur_state <- isolate(state_cache[[cur]])
      if (!is.null(mod_cur) && !is.null(cur_state)) {
        shinyjs::delay(70, mod_cur$restore_state(cur_state))
      }
    }
  }, ignoreInit = TRUE)
  
  iid <- function(tab_id, x) paste0(tab_id, "-", x)

  first_id <- add_individual()
  selected(first_id)
  last_selected(first_id)
  
  observeEvent(input$add_ind, {
    new_id <- add_individual()
    selected(new_id)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_ind_empty, {
    new_id <- add_individual()
    selected(new_id)
  }, ignoreInit = TRUE)
  
  observeEvent(input$rm_all, {
    ids <- active_ids()
    for (k in ids) mods[[k]] <- NULL
    cache_ids <- names(reactiveValuesToList(state_cache))
    for (k in cache_ids) state_cache[[k]] <- NULL
    active_ids(character(0))
    selected(NA_character_)
    last_selected(NULL)
  }, ignoreInit = TRUE)
  
  
  all_summaries <- reactive({
    ids <- active_ids()
    if (!length(ids)) return(list())
    res <- lapply(ids, function(k) {
      m <- mods[[k]]
      if (is.null(m)) return(NULL)
      out <- try(m$get_summary(), silent = TRUE)
      lab <- try(m$get_id(), silent = TRUE)
      if (inherits(out, "try-error") || inherits(lab, "try-error")) return(NULL)
      out$id <- if (nzchar(lab)) lab else k
      out
    })
    compact(res)
  })
  
  # Summary CSV download
  output$dl_summary <- downloadHandler(
    filename = function() sprintf("sys_severity_summary_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      res <- all_summaries()
      if (!length(res)) {
        write.csv(data.frame(id=character(), S_raw=numeric(), S_max=numeric(), S=numeric()),
                  file, row.names = FALSE); return()
      }
      df <- map_dfr(res, ~ tibble(
        id    = .x$id,
        S_raw = round(.x$S_raw, 2),
        S_max = round(.x$S_max, 2),
        S     = ifelse(is.na(.x$S), NA_real_, round(.x$S, 3))
      ))
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
