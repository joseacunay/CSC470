library(shiny)
library(httr)
library(jsonlite)
library(commonmark)
library(plotly)
library(DT)

# ── Claude API ──────────────────────────────────────────────────────────────

call_claude <- function(chat_history_df){
  system_prompt <- paste(
    "You are ScholarBot, an elite international scholarship advisor specialized in NCAA and NAIA recruiting.",
    "Formatting rules:",
    "- When explaining steps, requirements or recommendations use numbered lists automatically.",
    "- When comparing universities, leagues, scholarships, costs or options ALWAYS use markdown tables.",
    "- Tables must use proper markdown format with | separators and --- header dividers.",
    "- Do NOT wait for the user to ask for lists or tables.",
    "- Keep answers clear, structured and professional.",
    "- Use markdown formatting.",
    "- Be encouraging and motivating with student-athletes.",
    "- When mentioning scholarship amounts, always include dollar signs and commas."
  )

  messages_list <- lapply(1:nrow(chat_history_df), function(i){
    list(
      role    = ifelse(chat_history_df$sender[i] == "You", "user", "assistant"),
      content = chat_history_df$message[i]
    )
  })

  tryCatch({
    response <- POST(
      url = "https://api.anthropic.com/v1/messages",
      add_headers(
        "x-api-key"         = Sys.getenv("ANTHROPIC_API_KEY"),
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body = toJSON(list(
        model      = "claude-haiku-4-5-20251001",
        max_tokens = 1000,
        system     = system_prompt,
        messages   = messages_list
      ), auto_unbox = TRUE)
    )

    result <- content(response, as = "parsed", type = "application/json")

    if(!is.null(result$error)){
      return(paste("⚠️ API Error:", result$error$message))
    }

    paste(sapply(result$content, function(x) x$text), collapse = "\n")

  }, error = function(e){
    paste("⚠️ Could not connect to AI. Error:", conditionMessage(e))
  })
}

# ── Helper: markdown → HTML with proper table styling ────────────────────────

render_md <- function(text, accent = "#1a56db"){
  html <- commonmark::markdown_html(text)
  # inject inline styles into <table>, <th>, <td>, <tr>
  html <- gsub(
    "<table>",
    sprintf('<table style="border-collapse:collapse;width:100%%;margin:12px 0;font-size:13px;border-radius:8px;overflow:hidden;">'),
    html, fixed = TRUE
  )
  html <- gsub(
    "<thead>",
    "<thead>",
    html, fixed = TRUE
  )
  html <- gsub(
    "<th>",
    sprintf('<th style="background:%s;color:white;padding:9px 13px;text-align:left;font-weight:600;font-size:12px;white-space:nowrap;border:none;">', accent),
    html, fixed = TRUE
  )
  html <- gsub(
    "<td>",
    '<td style="padding:8px 13px;border-bottom:1px solid #334155;color:#cbd5e1;border-left:none;border-right:none;">',
    html, fixed = TRUE
  )
  html <- gsub(
    "<tr>",
    '<tr style="background:#1e293b;" onmouseover="this.style.background=\'#1e3a5f\'" onmouseout="this.style.background=\'#1e293b\'">',
    html, fixed = TRUE
  )
  HTML(html)
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  title = "ScholarBot",

  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap"),
    tags$style(HTML("
      * { box-sizing: border-box; }
      body {
        font-family: 'Inter', sans-serif;
        background: var(--bg-base, #0f172a);
        color: #e2e8f0;
        margin: 0; padding: 0;
        transition: background 0.3s;
      }

      /* ── Top navbar ── */
      .top-bar {
        background: var(--accent-gradient, linear-gradient(90deg,#1e3a5f,#1a56db));
        padding: 14px 30px;
        display: flex;
        align-items: center;
        gap: 14px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.4);
      }
      .top-bar .logo { font-size:26px; font-weight:700; color:white; letter-spacing:-0.5px; }
      .top-bar .logo span { color:#93c5fd; }
      .top-bar .tagline { font-size:12px; color:#bfdbfe; margin-top:2px; }
      .status-dot {
        width:10px; height:10px; background:#22c55e;
        border-radius:50%; margin-left:auto;
        box-shadow:0 0 8px #22c55e; animation:pulse 2s infinite;
      }
      @keyframes pulse { 0%,100%{opacity:1} 50%{opacity:0.4} }
      .status-label { font-size:11px; color:#86efac; margin-left:6px; }

      /* ── Layout ── */
      .main-wrapper {
        display:flex; gap:20px; padding:20px 24px;
        max-width:1400px; margin:0 auto;
      }
      .sidebar { width:230px; flex-shrink:0; }
      .sidebar-card {
        background: var(--bg-card, #1e293b);
        border:1px solid #334155; border-radius:14px;
        padding:16px; margin-bottom:14px;
      }
      .sidebar-card h5 {
        font-size:11px; font-weight:600;
        text-transform:uppercase; letter-spacing:1px;
        color:#64748b; margin:0 0 12px 0;
      }
      .quick-btn {
        display:block; width:100%;
        background:#0f172a; border:1px solid #334155;
        color:#cbd5e1; padding:8px 12px; border-radius:8px;
        font-size:12px; cursor:pointer; margin-bottom:6px;
        text-align:left; transition:all 0.2s;
        font-family:'Inter',sans-serif;
      }
      .quick-btn:hover { background:var(--accent,#1a56db); border-color:var(--accent,#1a56db); color:white; }
      .sport-tag {
        display:inline-block; background:#1e3a5f; color:#93c5fd;
        border-radius:20px; padding:3px 10px; font-size:11px;
        margin:3px 2px; border:1px solid #1a56db;
      }
      .stat-card {
        background:#0f172a; border:1px solid #334155;
        border-radius:12px; padding:14px; text-align:center; margin-bottom:8px;
      }
      .stat-card .stat-num { font-size:22px; font-weight:700; color:#93c5fd; }
      .stat-card .stat-lbl { font-size:11px; color:#64748b; margin-top:2px; }

      /* ── Theme panel ── */
      .theme-panel {
        background: var(--bg-card,#1e293b);
        border:1px solid #334155; border-radius:14px;
        padding:16px; margin-bottom:14px;
      }
      .theme-panel h5 {
        font-size:11px; font-weight:600;
        text-transform:uppercase; letter-spacing:1px;
        color:#64748b; margin:0 0 12px 0;
      }
      .theme-swatch {
        width:28px; height:28px; border-radius:50%;
        cursor:pointer; border:3px solid transparent;
        transition:all 0.2s; display:inline-block; margin:3px;
      }
      .theme-swatch:hover, .theme-swatch.active { border-color:white; transform:scale(1.15); }
      .theme-label { font-size:11px; color:#64748b; margin-bottom:6px; display:block; }

      /* ── Chat ── */
      .chat-wrapper { flex:1; min-width:0; }
      .chat-container {
        height:460px; overflow-y:auto;
        background: var(--bg-card,#1e293b);
        border:1px solid #334155; border-radius:16px;
        padding:20px; display:flex; flex-direction:column;
        gap:6px; scroll-behavior:smooth;
      }
      .chat-container::-webkit-scrollbar { width:6px; }
      .chat-container::-webkit-scrollbar-thumb { background:#334155; border-radius:3px; }

      .welcome-card {
        background:linear-gradient(135deg,#1e3a5f,#1a237e);
        border:1px solid #1a56db; border-radius:14px;
        padding:24px; text-align:center; margin:auto; max-width:400px;
      }
      .welcome-card .icon { font-size:48px; margin-bottom:10px; }
      .welcome-card h3 { margin:0 0 8px; font-size:20px; color:#e2e8f0; }
      .welcome-card p  { margin:0; font-size:13px; color:#94a3b8; line-height:1.6; }

      .msg-row-user { display:flex; justify-content:flex-end; margin:4px 0; }
      .msg-row-bot  { display:flex; justify-content:flex-start; align-items:flex-start; gap:10px; margin:4px 0; }
      .bot-avatar {
        width:34px; height:34px;
        background:linear-gradient(135deg,var(--accent,#1a56db),#7c3aed);
        border-radius:50%; display:flex; align-items:center;
        justify-content:center; font-size:16px; flex-shrink:0; margin-top:4px;
      }
      .msg-user {
        background:linear-gradient(135deg,var(--accent,#1a56db),#1d4ed8);
        color:white; padding:10px 16px;
        border-radius:18px 18px 4px 18px;
        max-width:70%; font-size:14px; line-height:1.5;
        box-shadow:0 2px 8px rgba(26,86,219,0.3);
      }
      .msg-bot {
        background:#334155; color:#e2e8f0;
        padding:12px 16px; border-radius:4px 18px 18px 18px;
        max-width:84%; font-size:14px; line-height:1.6;
        box-shadow:0 2px 8px rgba(0,0,0,0.2);
      }
      .msg-bot p  { margin:0 0 8px; }
      .msg-bot p:last-child { margin-bottom:0; }
      .msg-bot ul,
      .msg-bot ol { margin:6px 0; padding-left:20px; }
      .msg-bot li { margin-bottom:4px; }
      .msg-bot strong { color:#93c5fd; }
      .msg-bot code {
        background:#0f172a; padding:2px 6px;
        border-radius:4px; font-size:12px; color:#a78bfa;
      }

      /* Typing dots */
      .typing-indicator { display:flex; align-items:center; gap:10px; margin:4px 0; }
      .typing-dots {
        background:#334155; padding:12px 18px;
        border-radius:4px 18px 18px 18px;
        display:flex; gap:5px; align-items:center;
      }
      .typing-dots span {
        width:7px; height:7px; background:#64748b;
        border-radius:50%; animation:bounce 1.2s infinite;
      }
      .typing-dots span:nth-child(2) { animation-delay:0.2s; }
      .typing-dots span:nth-child(3) { animation-delay:0.4s; }
      @keyframes bounce {
        0%,60%,100%{ transform:translateY(0); background:#64748b; }
        30%         { transform:translateY(-6px); background:#93c5fd; }
      }

      /* Input bar */
      .input-bar { display:flex; gap:10px; margin-top:12px; align-items:flex-end; }
      .input-bar textarea {
        flex:1; background:#1e293b !important;
        border:1px solid #334155 !important; border-radius:12px !important;
        color:#e2e8f0 !important; padding:12px 16px !important;
        font-family:'Inter',sans-serif !important; font-size:14px !important;
        resize:none !important; outline:none !important; transition:border 0.2s !important;
      }
      .input-bar textarea:focus {
        border-color:var(--accent,#1a56db) !important;
        box-shadow:0 0 0 3px rgba(26,86,219,0.15) !important;
      }
      .input-bar textarea::placeholder { color:#64748b !important; }
      .send-btn {
        background:linear-gradient(135deg,var(--accent,#1a56db),#7c3aed) !important;
        border:none !important; color:white !important;
        padding:12px 22px !important; border-radius:12px !important;
        font-weight:600 !important; font-size:14px !important;
        cursor:pointer !important; transition:opacity 0.2s !important;
        height:48px; white-space:nowrap; font-family:'Inter',sans-serif !important;
      }
      .send-btn:hover { opacity:0.85 !important; }

      /* Analytics */
      .analytics-card {
        background:var(--bg-card,#1e293b); border:1px solid #334155;
        border-radius:16px; padding:20px; margin-top:20px;
      }
      .analytics-card h4 { margin:0 0 16px; font-size:15px; font-weight:600; color:#e2e8f0; }

      /* DT dark */
      .dataTables_wrapper { color:#e2e8f0 !important; }
      table.dataTable thead th {
        background:var(--accent,#1a56db) !important; color:white !important;
        border-bottom:2px solid #334155 !important;
        padding:10px 14px !important; font-size:12px !important;
      }
      table.dataTable tbody tr { background:#1e293b !important; color:#cbd5e1 !important; }
      table.dataTable tbody tr:nth-child(even) { background:#263248 !important; }
      table.dataTable tbody tr:hover { background:#1e3a5f !important; }
      table.dataTable tbody td { border-color:#334155 !important; padding:9px 14px !important; font-size:13px !important; }
      .dataTables_info, .dataTables_length { color:#64748b !important; font-size:12px !important; }
      .dataTables_filter input {
        background:#0f172a !important; border:1px solid #334155 !important;
        color:#e2e8f0 !important; border-radius:6px !important; padding:4px 8px !important;
      }
      .paginate_button.current {
        background:var(--accent,#1a56db) !important;
        color:white !important; border-radius:6px !important;
      }
    "))
  ),

  # ── Top bar ─────────────────────────────────────────────
  div(class = "top-bar",
    div(
      div(class = "logo", "🎓 Scholar", tags$span("Bot")),
      div(class = "tagline", "NCAA & NAIA International Recruiting Advisor")
    ),
    div(class = "status-dot"),
    div(class = "status-label", "AI Online")
  ),

  # ── Main wrapper ─────────────────────────────────────────
  div(class = "main-wrapper",

    # ── Sidebar ──────────────────────────────────────────
    div(class = "sidebar",

      # Theme customizer
      div(class = "theme-panel",
        tags$h5("🎨 Customize"),
        tags$span(class = "theme-label", "Accent Color"),
        div(
          tags$div(class="theme-swatch active", id="sw-blue",
                   style="background:#1a56db;",
                   onclick="setTheme('#1a56db','linear-gradient(90deg,#1e3a5f,#1a56db)','sw-blue')"),
          tags$div(class="theme-swatch", id="sw-violet",
                   style="background:#7c3aed;",
                   onclick="setTheme('#7c3aed','linear-gradient(90deg,#2e1065,#7c3aed)','sw-violet')"),
          tags$div(class="theme-swatch", id="sw-emerald",
                   style="background:#059669;",
                   onclick="setTheme('#059669','linear-gradient(90deg,#064e3b,#059669)','sw-emerald')"),
          tags$div(class="theme-swatch", id="sw-rose",
                   style="background:#e11d48;",
                   onclick="setTheme('#e11d48','linear-gradient(90deg,#4c0519,#e11d48)','sw-rose')"),
          tags$div(class="theme-swatch", id="sw-amber",
                   style="background:#d97706;",
                   onclick="setTheme('#d97706','linear-gradient(90deg,#451a03,#d97706)','sw-amber')"),
          tags$div(class="theme-swatch", id="sw-cyan",
                   style="background:#0891b2;",
                   onclick="setTheme('#0891b2','linear-gradient(90deg,#164e63,#0891b2)','sw-cyan')")
        ),
        br(),
        tags$span(class = "theme-label", "Background"),
        div(
          tags$div(class="theme-swatch active", id="bg-dark",
                   style="background:#0f172a; border:1px solid #334155;",
                   onclick="setBg('#0f172a','#1e293b','bg-dark')"),
          tags$div(class="theme-swatch", id="bg-darker",
                   style="background:#020617; border:1px solid #334155;",
                   onclick="setBg('#020617','#0f172a','bg-darker')"),
          tags$div(class="theme-swatch", id="bg-gray",
                   style="background:#111827; border:1px solid #374151;",
                   onclick="setBg('#111827','#1f2937','bg-gray')")
        )
      ),

      # Quick questions
      div(class = "sidebar-card",
        tags$h5("Quick Questions"),
        tags$button("🏫 Find my university",    class="quick-btn",
                    onclick="sendQ('What universities match my profile?')"),
        tags$button("💰 Scholarship info",       class="quick-btn",
                    onclick="sendQ('What scholarships are available for international athletes?')"),
        tags$button("⚖️ Compare NCAA vs NAIA",  class="quick-btn",
                    onclick="sendQ('Compare NCAA and NAIA in a detailed table')"),
        tags$button("📋 Recruiting steps",       class="quick-btn",
                    onclick="sendQ('What are the steps to get recruited?')"),
        tags$button("🏆 Top D1 volleyball",      class="quick-btn",
                    onclick="sendQ('Compare top 5 NCAA D1 schools for volleyball with scholarship amounts in a table')"),
        tags$button("📊 Show analytics",         class="quick-btn",
                    onclick="sendQ('Show me the analytics dashboard')")
      ),

      # Sports
      div(class = "sidebar-card",
        tags$h5("Sports Covered"),
        HTML("<span class='sport-tag'>⚽ Soccer</span>
              <span class='sport-tag'>🏀 Basketball</span>
              <span class='sport-tag'>🎾 Tennis</span>
              <span class='sport-tag'>🏊 Swimming</span>
              <span class='sport-tag'>🏈 Football</span>
              <span class='sport-tag'>⚾ Baseball</span>
              <span class='sport-tag'>🏐 Volleyball</span>
              <span class='sport-tag'>🤸 Track</span>")
      ),

      # Stats
      div(class = "sidebar-card",
        tags$h5("Stats at a Glance"),
        div(class="stat-card", div(class="stat-num","1,100+"), div(class="stat-lbl","NCAA Schools")),
        div(class="stat-card", div(class="stat-num","$3.6B"),  div(class="stat-lbl","Aid Awarded/yr")),
        div(class="stat-card", div(class="stat-num","500+"),   div(class="stat-lbl","NAIA Schools"))
      )
    ),

    # ── Chat + analytics ─────────────────────────────────
    div(class = "chat-wrapper",

      div(class="chat-container", id="chat-box",
          uiOutput("chat_ui")),

      div(class="input-bar",
          textAreaInput("user_input", NULL, rows=2,
                        placeholder="Ask me anything... (Enter to send, Shift+Enter for new line)"),
          actionButton("send", "Send ➤", class="send-btn")
      ),

      conditionalPanel(
        condition = "output.showPlot == true",
        div(class="analytics-card",
          tags$h4("📊 Athlete Analytics Dashboard"),
          fluidRow(
            column(4, plotlyOutput("radar_plot",   height="280px")),
            column(4, plotlyOutput("bar_plot",     height="280px")),
            column(4, plotlyOutput("scatter_plot", height="280px"))
          ),
          br(),
          DTOutput("scholarship_table")
        )
      )
    )
  ),

  # ── Scripts ──────────────────────────────────────────────
  tags$script(HTML("
    // shortcut to send a quick question
    function sendQ(text){
      $('#user_input').val(text);
      $('#send').click();
    }

    // auto-scroll
    Shiny.addCustomMessageHandler('scrollChat', function(msg){
      var el = document.getElementById('chat-box');
      if(el) el.scrollTop = el.scrollHeight;
    });

    // typing indicator
    Shiny.addCustomMessageHandler('setTyping', function(show){
      var el = document.getElementById('typing-row');
      if(el) el.style.display = show ? 'flex' : 'none';
    });

    // Enter to send
    $(document).on('keydown','#user_input',function(e){
      if(e.key==='Enter' && !e.shiftKey){ e.preventDefault(); $('#send').click(); }
    });

    // ── Theme functions ──
    function setTheme(accent, gradient, swId){
      document.documentElement.style.setProperty('--accent', accent);
      document.documentElement.style.setProperty('--accent-gradient', gradient);
      document.querySelectorAll('.theme-swatch').forEach(function(el){
        if(['sw-blue','sw-violet','sw-emerald','sw-rose','sw-amber','sw-cyan'].includes(el.id))
          el.classList.remove('active');
      });
      document.getElementById(swId).classList.add('active');
      // update DT header color
      document.querySelectorAll('table.dataTable thead th').forEach(function(th){
        th.style.background = accent;
      });
    }

    function setBg(base, card, bgId){
      document.body.style.background = base;
      document.documentElement.style.setProperty('--bg-base', base);
      document.documentElement.style.setProperty('--bg-card', card);
      document.querySelectorAll('.sidebar-card, .chat-container, .analytics-card, .theme-panel').forEach(function(el){
        el.style.background = card;
      });
      document.querySelectorAll('#bg-dark,#bg-darker,#bg-gray').forEach(function(el){
        el.classList.remove('active');
      });
      document.getElementById(bgId).classList.add('active');
    }
  "))
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session){

  chat_history <- reactiveVal(
    data.frame(sender=character(0), message=character(0), stringsAsFactors=FALSE)
  )
  show_plot <- reactiveVal(FALSE)
  is_typing <- reactiveVal(FALSE)

  observeEvent(input$send, {
    req(input$user_input)
    user_msg <- trimws(input$user_input)
    if(nchar(user_msg) == 0) return()
    user_lc <- tolower(user_msg)

    hist <- rbind(
      chat_history(),
      data.frame(sender="You", message=user_msg, stringsAsFactors=FALSE),
      make.row.names=FALSE
    )
    chat_history(hist)
    updateTextAreaInput(session, "user_input", value="")
    session$sendCustomMessage("scrollChat", list())

    show_plot(grepl("stats|graph|chart|show|analytic|dashboard|visual", user_lc))

    is_typing(TRUE)
    session$sendCustomMessage("setTyping", TRUE)
    session$sendCustomMessage("scrollChat", list())

    bot <- tryCatch({
      if(user_lc %in% c("hi","hello","hey")){
        "👋 Hey! I'm **ScholarBot**, your personal NCAA & NAIA recruiting advisor.\n\nWhat sport do you play, and what country are you from?"
      } else if(grepl("thank", user_lc)){
        "You're welcome! 🎓 Feel free to ask anything else about your recruiting journey!"
      } else if(show_plot()){
        "📊 Here's your **Athlete Analytics Dashboard** below!\n\n- **Skills Radar** — your athletic profile\n- **Scholarship by Division** — average awards\n- **GPA vs Scholarship** — academic performance vs aid"
      } else {
        resp <- call_claude(hist)
        if(is.null(resp) || length(resp)==0 || !is.character(resp))
          "⚠️ Could not generate a response. Please try again."
        else
          as.character(resp[[1]])
      }
    }, error=function(e) paste("⚠️ Error:", conditionMessage(e)))

    is_typing(FALSE)
    session$sendCustomMessage("setTyping", FALSE)

    hist <- rbind(hist,
                  data.frame(sender="Bot", message=bot, stringsAsFactors=FALSE),
                  make.row.names=FALSE)
    chat_history(hist)
    session$sendCustomMessage("scrollChat", list())
  })

  # ── Chat UI ──────────────────────────────────────────────
  output$chat_ui <- renderUI({
    msgs <- chat_history()

    base <- if(nrow(msgs)==0){
      list(div(class="welcome-card",
        div(class="icon","🎓"),
        tags$h3("Welcome to ScholarBot"),
        tags$p("Your AI advisor for NCAA & NAIA international recruiting.",
               tags$br(),
               "Ask me about scholarships, universities, or the recruiting process.",
               tags$br(), tags$br(),
               tags$em(style="color:#64748b;font-size:12px;",
                       "💡 Enter to send · Shift+Enter for new line"))
      ))
    } else {
      lapply(1:nrow(msgs), function(i){
        if(msgs$sender[i]=="You"){
          div(class="msg-row-user",
            div(class="msg-user", render_md(msgs$message[i]))
          )
        } else {
          div(class="msg-row-bot",
            div(class="bot-avatar","🤖"),
            div(class="msg-bot", render_md(msgs$message[i]))
          )
        }
      })
    }

    typing_row <- div(
      id="typing-row", class="typing-indicator",
      style=if(is_typing()) "display:flex;" else "display:none;",
      div(class="bot-avatar","🤖"),
      div(class="typing-dots", tags$span(), tags$span(), tags$span())
    )

    tagList(c(base, list(typing_row)))
  })

  output$showPlot <- renderText({ if(show_plot()) "true" else "false" })
  outputOptions(output, "showPlot", suspendWhenHidden=FALSE)

  # ── Radar ─────────────────────────────────────────────────
  output$radar_plot <- renderPlotly({
    cats <- c("Speed","Strength","Agility","Stamina","Technique","Game IQ")
    vals <- c(88,75,92,83,90,87)
    plot_ly(type="scatterpolar", mode="lines+markers",
            r=c(vals,vals[1]), theta=c(cats,cats[1]),
            fill="toself", fillcolor="rgba(26,86,219,0.20)",
            line=list(color="#1a56db",width=2.5),
            marker=list(color="#93c5fd",size=8,line=list(color="#1a56db",width=1.5))) |>
      layout(paper_bgcolor="#1e293b", plot_bgcolor="#1e293b",
             polar=list(bgcolor="#0f172a",
                        radialaxis=list(visible=TRUE,range=c(0,100),
                                        color="#64748b",gridcolor="#334155",
                                        tickfont=list(color="#64748b",size=9)),
                        angularaxis=list(color="#94a3b8",
                                         tickfont=list(color="#94a3b8",size=11))),
             title=list(text="<b>Skill Profile</b>",
                        font=list(color="#e2e8f0",size=13),x=0.5),
             margin=list(t=45,b=20,l=40,r=40),showlegend=FALSE) |>
      config(displayModeBar=FALSE)
  })

  # ── Bar ───────────────────────────────────────────────────
  output$bar_plot <- renderPlotly({
    divs  <- c("NCAA D1","NCAA D2","NCAA D3","NAIA","JUCO")
    schol <- c(28500,15200,6800,12400,9100)
    plot_ly(x=~divs, y=~schol, type="bar",
            text=~paste0("$",formatC(schol,format="d",big.mark=",")),
            textposition="outside",
            textfont=list(color="#93c5fd",size=11),
            marker=list(color=c("#1a56db","#2563eb","#3b82f6","#7c3aed","#a78bfa"),
                        line=list(color="#0f172a",width=1.5)),
            hovertemplate="<b>%{x}</b><br>$%{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="#1e293b", plot_bgcolor="#1e293b",
             title=list(text="<b>Avg Scholarship / Division</b>",
                        font=list(color="#e2e8f0",size=13),x=0.5),
             xaxis=list(color="#94a3b8",gridcolor="transparent",
                        tickfont=list(color="#94a3b8",size=11),zeroline=FALSE),
             yaxis=list(color="#94a3b8",gridcolor="#1e293b",
                        tickprefix="$",tickformat=",.0f",
                        tickfont=list(color="#64748b",size=10),zeroline=FALSE),
             margin=list(t=50,b=40,l=65,r=20),bargap=0.35) |>
      config(displayModeBar=FALSE)
  })

  # ── Scatter ───────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    set.seed(42)
    n   <- 60
    gpa <- round(runif(n,2.0,4.0),2)
    award <- pmax(0,pmin(5000+(gpa-2)*9000+rnorm(n,0,2500),36000))
    div   <- sample(c("NCAA D1","NCAA D2","NAIA","JUCO"),n,replace=TRUE)
    cols  <- c("NCAA D1"="#1a56db","NCAA D2"="#3b82f6","NAIA"="#7c3aed","JUCO"="#a78bfa")
    plot_ly(x=~gpa,y=~award,type="scatter",mode="markers",
            color=~div,colors=cols,
            marker=list(size=8,opacity=0.8,line=list(color="#0f172a",width=1)),
            text=~paste0("<b>",div,"</b><br>GPA:",gpa,
                         "<br>Award:$",formatC(round(award),format="d",big.mark=",")),
            hovertemplate="%{text}<extra></extra>") |>
      layout(paper_bgcolor="#1e293b", plot_bgcolor="#0f172a",
             title=list(text="<b>GPA vs Scholarship Award</b>",
                        font=list(color="#e2e8f0",size=13),x=0.5),
             xaxis=list(title=list(text="GPA",font=list(color="#94a3b8",size=11)),
                        color="#94a3b8",gridcolor="#1e293b",
                        tickfont=list(color="#94a3b8",size=10),zeroline=FALSE,range=c(1.8,4.2)),
             yaxis=list(title=list(text="Award (USD)",font=list(color="#94a3b8",size=11)),
                        color="#94a3b8",gridcolor="#1e293b",
                        tickprefix="$",tickformat=",.0f",
                        tickfont=list(color="#64748b",size=10),zeroline=FALSE),
             legend=list(font=list(color="#94a3b8",size=11),bgcolor="transparent",x=0.01,y=0.99),
             margin=list(t=50,b=50,l=70,r=20)) |>
      config(displayModeBar=FALSE)
  })

  # ── DT table ──────────────────────────────────────────────
  output$scholarship_table <- renderDT({
    df <- data.frame(
      Division      = c("NCAA D1","NCAA D2","NCAA D3","NAIA","JUCO"),
      `Avg Award`   = c("$28,500","$15,200","$6,800","$12,400","$9,100"),
      `# Schools`   = c(350,308,443,241,525),
      `Int'l Spots` = c("High","Medium","Low","High","Medium"),
      `GPA Min`     = c("2.3","2.0","2.0","2.0","2.0"),
      `Roster Limit`= c("13","13","No limit","15","No limit"),
      check.names   = FALSE
    )
    datatable(df,
              options=list(dom="ftp",pageLength=5,ordering=TRUE,scrollX=TRUE),
              rownames=FALSE, class="display compact hover") |>
      formatStyle("Int'l Spots",
                  color=styleEqual(c("High","Medium","Low"),c("#4ade80","#facc15","#f87171")),
                  fontWeight="bold") |>
      formatStyle("Avg Award", color="#93c5fd", fontWeight="600") |>
      formatStyle(0, target="row", backgroundColor="#1e293b", color="#cbd5e1")
  })
}

shinyApp(ui, server)
