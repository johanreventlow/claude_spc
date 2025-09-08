# ui_welcome_page.R
# UI komponenter for velkomstside

# Dependencies ----------------------------------------------------------------

# UI VELKOMSTSIDE KOMPONENTER =================================================

## Hovedfunktion for velkomstside
# Opretter komplet velkomstside med hero sektion og handlingsknapper
create_welcome_page <- function() {
  div(
    class = "welcome-page",
    style = "min-height: 100vh; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",
    
    # Hero Sektion
    div(
      class = "hero-section py-5",
      div(
        class = "container-fluid",
        div(
          class = "row align-items-center mb-5",
          div(
            class = "col-12 text-center",
            h1(
              class = "display-4 fw-bold",
              style = paste0("color: ", HOSPITAL_COLORS$primary, "; margin-bottom: 1rem;"),
              "Velkommen til BFH SPC-værktøj"
            ),
            p(
              class = "lead text-muted",
              style = "font-size: 1.25rem; max-width: 800px; margin: 0 auto;",
              "Transformér dine data til indsigter med Statistical Process Control. ", 
              "Identificer mønstre, spot trends og træf bedre beslutninger baseret på dine healthcare data."
            )
          )
        )
      )
    ),
    
    # Main Content - Two Column Layout
    div(
      class = "container-fluid px-4",
      div(
        class = "row g-4 mb-5",
        
        # LEFT COLUMN - Getting Started Guide
        div(
          class = "col-lg-6",
          create_getting_started_card()
        ),
        
        # RIGHT COLUMN - Understanding SPC
        div(
          class = "col-lg-6", 
          create_understanding_spc_card()
        )
      )
    ),
    
    # Call to Action Section
    div(
      class = "cta-section py-5",
      style = paste0("background-color: ", HOSPITAL_COLORS$primary, "; color: white;"),
      div(
        class = "container text-center",
        div(
          class = "row",
          div(
            class = "col-lg-8 mx-auto",
            h2(class = "mb-4", "Klar til at komme i gang?"),
            p(class = "mb-4 fs-5", "Start din første SPC-analyse på under 5 minutter."),
            div(
              class = "d-grid gap-2 d-md-flex justify-content-md-center",
              actionButton(
                "start_new_session",
                "🚀 Start ny analyse",
                class = "btn btn-light btn-lg me-md-2",
                style = "font-weight: 600; padding: 12px 30px;"
              ),
              actionButton(
                "upload_data_welcome",  
                "📊 Upload data",
                class = "btn btn-outline-light btn-lg",
                style = "font-weight: 600; padding: 12px 30px;"
              )
            )
          )
        )
      )
    )
  )
}

# Left Column - Getting Started Guide
create_getting_started_card <- function() {
  card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    card_header(
      class = "bg-white border-0 pb-0",
      style = "border-radius: 15px 15px 0 0;",
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-3",
          style = paste0("background: ", HOSPITAL_COLORS$primary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          icon("rocket", style = "color: white; font-size: 1.5rem;")
        ),
        div(
          h3(class = "card-title mb-1", "Kom i gang på 3 trin"),
          p(class = "text-muted mb-0", "Din vej fra data til indsigt")
        )
      )
    ),
    card_body(
      class = "px-4 py-4",
      
      # Step 1
      create_step_item(
        number = "1",
        icon = "upload",
        title = "Upload dine data",
        description = "Excel (.xlsx/.xls) eller CSV-fil med kolonneoverskrifter. Vi understøtter danske tal og datoformater.",
        example = "Eksempel: Dato, Tæller, Nævner, Kommentar"
      ),
      
      # Step 2
      create_step_item(
        number = "2", 
        icon = "sliders-h",
        title = "Konfigurer din analyse",
        description = "Automatisk kolonnedetektering eller manuel opsætning. Vælg chart type baseret på dine data.",
        example = "Run chart, P-chart, U-chart, X̄-chart"
      ),
      
      # Step 3
      create_step_item(
        number = "3",
        icon = "chart-line",
        title = "Få dine insights",
        description = "Interaktiv SPC-graf med centerlinjer, kontrolgrænser og specialle mønstre (Anhøj regler).",
        example = "Eksporter som Excel, PDF eller PNG"
      ),
      
      # Quick Start Button
      div(
        class = "mt-4 pt-3 border-top",
        div(
          class = "d-grid",
          actionButton(
            "quick_start_demo",
            "👆 Prøv med eksempel-data",
            class = "btn btn-outline-primary btn-lg",
            style = "border-radius: 10px; font-weight: 500;"
          )
        ),
        p(
          class = "text-center text-muted mt-2 mb-0",
          style = "font-size: 0.9rem;",
          "Eller upload dine egne data direkte"
        )
      )
    )
  )
}

# Right Column - Understanding SPC
create_understanding_spc_card <- function() {
  card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    card_header(
      class = "bg-white border-0 pb-0", 
      style = "border-radius: 15px 15px 0 0;",
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-3",
          style = paste0("background: ", HOSPITAL_COLORS$secondary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          icon("lightbulb", style = "color: white; font-size: 1.5rem;")
        ),
        div(
          h3(class = "card-title mb-1", "Forstå SPC"),
          p(class = "text-muted mb-0", "Værktøjet der transformerer data til handling")
        )
      )
    ),
    card_body(
      class = "px-4 py-4",
      
      # What is SPC?
      create_info_section(
        icon = "question-circle",
        title = "Hvad er Statistical Process Control?",
        content = "SPC hjælper dig med at skelne mellem normal variation og særlige årsager i dine processer. I sundhedsvæsenet betyder det bedre patientpleje gennem data-drevet beslutningstagning."
      ),
      
      # Why SPC in Healthcare?
      create_info_section(
        icon = "heartbeat",
        title = "Hvorfor SPC i sundhedsvæsenet?",
        content = HTML("
          <ul class='list-unstyled'>
            <li><strong>🎯 Spot trends tidligt:</strong> Identificer problemer før de bliver kritiske</li>
            <li><strong>📊 Forstå variation:</strong> Normal udsving vs. særlige årsager</li>
            <li><strong>💡 Træf bedre beslutninger:</strong> Baseret på statistisk evidens</li>
            <li><strong>🚀 Forbedre kontinuerligt:</strong> Måle effekt af ændringer</li>
          </ul>
        ")
      ),
      
      # Healthcare Examples
      create_info_section(
        icon = "hospital",
        title = "Konkrete eksempler fra BFH",
        content = HTML("
          <div class='row g-2'>
            <div class='col-6'>
              <div class='example-item p-2 rounded' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Infektionsrater</small><br>
                <small class='text-muted'>Monitor og reducer HAI</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Ventetider</small><br>
                <small class='text-muted'>Optimér patientflow</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded mt-2' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Medicinfejl</small><br>
                <small class='text-muted'>Forbedre patientsikkerhed</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded mt-2' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Genindlæggelser</small><br>
                <small class='text-muted'>Kvalitetsindikatorer</small>
              </div>
            </div>
          </div>
        ")
      )
    )
  )
}

# Helper function for step items
create_step_item <- function(number, icon, title, description, example = NULL) {
  div(
    class = "step-item d-flex mb-4",
    # Step Number Circle
    div(
      class = "step-number me-3 flex-shrink-0",
      style = paste0("width: 40px; height: 40px; background: ", HOSPITAL_COLORS$primary, "; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 1.1rem;"),
      number
    ),
    # Step Content
    div(
      class = "step-content flex-grow-1",
      div(
        class = "d-flex align-items-center mb-2",
        icon(icon, class = "text-primary me-2"),
        h5(class = "mb-0 fw-semibold", title)
      ),
      p(class = "text-muted mb-1", description),
      if (!is.null(example)) {
        p(class = "small text-primary mb-0", 
          style = "font-style: italic;",
          example)
      }
    )
  )
}

# Helper function for info sections
create_info_section <- function(icon, title, content) {
  div(
    class = "info-section mb-4",
    div(
      class = "d-flex align-items-start mb-2",
      div(
        class = "me-2 flex-shrink-0",
        icon(icon, class = "text-secondary", style = "font-size: 1.2rem; margin-top: 2px;")
      ),
      div(
        class = "flex-grow-1",
        h5(class = "fw-semibold mb-2", title),
        div(class = "text-muted", content)
      )
    )
  )
}