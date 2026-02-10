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

to_plotly_dark <- function(p, tooltip = "text", modebar = FALSE, legend_title = NULL,
                           x_title = NULL, y_title = NULL, show_x_ticks = TRUE, show_y_ticks = TRUE) {
  ggplotly(p, tooltip = tooltip) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = "#cfd4da"),
      legend = list(
        orientation = "v",
        x = 1.02, xanchor = "left",
        y = 0.5,
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#cfd4da"),
        title = list(text = legend_title %||% "", font = list(color = "#cfd4da"))
      ),
      xaxis = list(
        title = x_title %||% "",
        titlefont=list(color="#cfd4da"),
        tickfont=list(color="#cfd4da"),
        showgrid=FALSE, zeroline=FALSE,
        showticklabels = show_x_ticks
      ),
      yaxis = list(
        title = y_title %||% "",
        titlefont=list(color="#cfd4da"),
        tickfont=list(color="#cfd4da"),
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
    .hint{ color:#adb5bd; }
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
      bslib::card_header("Respiratory / Sleep"),
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
      to_plotly_dark(p, tooltip = "text", legend_title = "Symptom",
                     x_title = NULL, y_title = NULL,
                     show_x_ticks = TRUE, show_y_ticks = TRUE)
    })
    
    output$summary_ui <- renderUI({
      sv <- summary_vals()
      if (!sv$tau_ok) {
        return(tagList(
          p(HTML(sprintf("<b>S<sub>raw</sub></b> = %.2f", sv$S_raw))),
          p(HTML(sprintf("<b>S<sub>max</sub></b> = %.2f", sv$S_max))),
          div(class = "alert alert-warning", HTML(sprintf(
            "Not enough information yet: <b>S<sub>max</sub>=%.2f</b> is below τ=10. Provide more details to compute S.", sv$S_max)))
        ))
      }
      tagList(
        div(class = "score-big", HTML(sprintf("S = %.3f", sv$S))),
        tags$div(HTML(sprintf("<b>S<sub>raw</sub></b> = %.2f &nbsp;&nbsp; <b>S<sub>max</sub></b> = %.2f (τ=10)",
                              sv$S_raw, sv$S_max)))
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
        html, body, .container-fluid{
          background-color:#171a1e !important;
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
          background:#4dabf7;
          opacity:0.7;
        }
        .app-header h1{
          font-family: inherit;
          font-weight: 600;
          font-size: 1.25rem;
          text-transform: uppercase;
          letter-spacing: .03em;
          margin: 0;
          color: inherit;
        }
        @media (max-width: 768px){ .app-header h1{ font-size: 1rem; } }

        .page-container{max-width:1180px;margin:0 auto;padding:0 14px;}

        a{ color:#4dabf7; text-decoration: none; }
        a:hover, a:focus{ color:#74c0fc; text-decoration: none; }
        .site-footer a{ color:#adb5bd !important; text-decoration:none; }
        .site-footer a:hover, .site-footer a:focus{ color:#4dabf7 !important; text-decoration:none; }

        .card{border:1px solid rgba(255,255,255,0.08)!important;border-radius:10px!important;background:#202326;}
        .card-header{background:#2a2e32!important;border-bottom:1px solid rgba(255,255,255,0.06)!important;color:#cfd4da;padding:10px 14px!important;font-weight:600;}
        .card-body{padding:14px!important;}
        .card-footer{background:#202326;border-top:1px solid rgba(255,255,255,.06);padding:12px 14px;}

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

        .muted{color:#adb5bd;}
        h4{font-size:1.05rem;font-weight:600;color:#cfd4da;margin-top:0;}

        .toolbar-bar{display:flex;flex-wrap:wrap;align-items:center;gap:8px;margin-bottom:14px;padding:0;background:transparent;border:none;}
        .toolbar-bar .toolbar-spacer{flex:1;min-width:8px;}
        .toolbar-bar .form-group{margin-bottom:0;flex-shrink:0;min-width:28ch;}
        .toolbar-bar select{min-width:100%;max-width:100%;padding-right:2.25em;box-sizing:border-box;background:#000 !important;border-color:#333 !important;color:#cfd4da !important;}
        .toolbar-bar select:focus{background:#000 !important;border-color:#555 !important;color:#cfd4da !important;}
        .toolbar-bar select option{background:#000 !important;color:#cfd4da !important;}
        .selectize-dropdown{background:#000 !important;border:1px solid #333 !important;}
        .selectize-dropdown .option,.selectize-dropdown .optgroup-header{background:#000 !important;color:#cfd4da !important;}
        .selectize-dropdown .option.active,.selectize-dropdown .option:hover{background:#1a1a1a !important;color:#cfd4da !important;}
        .selectize-input{background:#000 !important;border-color:#333 !important;color:#cfd4da !important;}
        .selectize-input input{color:#cfd4da !important;}
        .selectize-input.focus{border-color:#555 !important;box-shadow:0 0 0 0.2rem rgba(85,85,85,0.25);}
        .toolbar-bar .btn{margin:0;}
        #add_ind.btn-primary{
          background-color:#f8f9fa !important;
          border-color:#f8f9fa !important;
          color:#1b1f23 !important;
          font-weight:600;
        }
        #add_ind.btn-primary:hover,
        #add_ind.btn-primary:focus{
          background-color:#e9ecef !important;
          border-color:#e9ecef !important;
          color:#14181f !important;
        }
        #add_ind.btn-primary:focus{
          box-shadow:0 0 0 0.2rem rgba(248,249,250,0.25) !important;
        }
        .compute-wrap{width:100%;}
        .compute-wrap > .shiny-input-container{width:100%;margin-bottom:0;}
        button[id$='-compute']{
          background-color:#f8f9fa !important;
          border-color:#f8f9fa !important;
          color:#1b1f23 !important;
          font-weight:700;
          width:100%;
          display:block;
        }
        button[id$='-compute']:hover,
        button[id$='-compute']:focus{
          background-color:#e9ecef !important;
          border-color:#e9ecef !important;
          color:#14181f !important;
        }
        button[id$='-compute']:focus{
          box-shadow:0 0 0 0.2rem rgba(248,249,250,0.25) !important;
        }
        .main-view{transition:opacity 120ms ease-in-out;}

        .form-control, .form-select, input[type=\"text\"], input[type=\"number\"], select, textarea{
          background-color:#0d0d0d !important;
          border-color:#333 !important;
          color:#cfd4da !important;
        }
        .form-control:focus, .form-select:focus, input:focus, select:focus, textarea:focus{
          background-color:#0d0d0d !important;
          border-color:#555 !important;
          color:#cfd4da !important;
          box-shadow:0 0 0 0.2rem rgba(85,85,85,0.25);
        }
        .form-control::placeholder{color:#6c757d;}
        input[type=\"number\"]::-webkit-inner-spin-button,
        input[type=\"number\"]::-webkit-outer-spin-button{opacity:1;filter:invert(1);background:#333;border-radius:2px;}
        select.form-control option, .form-select option{background:#0d0d0d;color:#cfd4da;}
        .form-check-input{background-color:#0d0d0d !important;border-color:#555 !important;}
        .form-check-input:checked{background-color:#375a7f;border-color:#375a7f;}
        .form-check-input:focus{border-color:#555;box-shadow:0 0 0 0.2rem rgba(85,85,85,0.25);}

        .site-footer__inner{max-width:1180px;margin:0 auto;padding:20px 14px;display:flex;align-items:center;justify-content:space-between;gap:14px;}
        .site-footer__logo{height:60px;flex:0 0 auto;}
        .site-footer__center{flex:1 1 auto;text-align:center;color:#adb5bd;}
        .site-footer{position:relative;z-index:10;}
        @media (max-width:768px){.site-footer__logo{height:42px;}.site-footer__center{font-size:0.88rem;}}

        .bib-link{cursor:pointer;text-decoration:underline;color:#4dabf7;font-size:inherit;}
        .bib-link:hover,.bib-link:focus{color:#74c0fc;text-decoration:underline;}
        #bibtex_block{white-space:pre-wrap;font-size:.86rem;background:#1f1f1f;padding:10px;border-radius:8px;border:1px solid #2a2a2a;color:#cfd4da;}
      ")),
      tags$script(HTML("
        document.addEventListener('click', function(e){
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
      "))
    ),
    tags$header(
      class = "app-header",
      tags$h1("Schaaf-Yang Syndrome Severity Score Calculator")
    ),
    div(class = "page-container",
        sidebarLayout(
          sidebarPanel(
            width = 3, class = "sidebar",
            downloadButton("dl_summary", "Download summary", class = "btn btn-dark sidebar-download"),
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
                       "We are deeply grateful to individuals with SYS and their families for their time, trust, and contributions."),
                tags$hr(style = "opacity:0.25;"),
                tags$div(
                  class = "muted",
                  style = "font-size:0.9em; margin-bottom:8px;",
                  tags$span("Please cite: Schubert et al. (2025) · "),
                  actionLink("show_bib", "BibTeX", class = "bib-link")
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
    style = "background:transparent; color:#cfd4da;",
    tags$div(
      class = "site-footer__inner",
      tags$img(src = "https://lh3.googleusercontent.com/d/12vCwvlmN8xrKnlU1GsDGJeDBgq_yl0sM", class = "site-footer__logo"),
      tags$div(
        class = "site-footer__center",
        tags$p(style = "margin:0;",
               tags$span(HTML("&copy; 2026 ")),
               tags$a(href = "https://tim-schubert.github.io", target = "_blank", rel = "noopener noreferrer", "Tim Schubert"),
               tags$span(HTML(" &middot; ")),
               tags$a(href = "https://www.apache.org/licenses/LICENSE-2.0.html", target = "_blank", "Apache License 2.0")
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
