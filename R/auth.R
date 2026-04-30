library(httr)
library(jsonlite)
library(shiny)
library(bslib)

auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(sprintf("
      $(document).ready(function(){
        // Enter key for Login
        $('#%1$s, #%2$s').on('keypress', function(e){
          if(e.keyCode === 13){ e.preventDefault(); $('#%3$s').click(); }
        });
        
        // Enter key for Register
        $('#%4$s, #%5$s, #%6$s, #%7$s, #%8$s, #%9$s, #%10$s').on('keypress', function(e){
          if(e.keyCode === 13){ e.preventDefault(); $('#%11$s').click(); }
        });
      });

      // THE FIX: Wait until Shiny is completely connected before looking for the token!
      $(document).on('shiny:connected', function() {
        var hash = window.location.hash;
        if (hash && hash.includes('type=recovery')) {
          // Supabase uses standard query string formatting after the hash
          var params = new URLSearchParams(hash.substring(1));
          var token = params.get('access_token');
          
          if (token) {
            // Now Shiny is actually listening!
            Shiny.setInputValue('%12$s', token);
            // Erase the token from the browser URL bar
            history.replaceState(null, null, ' '); 
          }
        }
      });
    ", 
                             ns("login_email"), ns("login_pwd"), ns("btn_login"),
                             ns("reg_fname"), ns("reg_lname"), ns("reg_agency"), ns("reg_job"), ns("reg_phone"), ns("reg_email"), ns("reg_pwd"), ns("btn_register"),
                             ns("recovery_token")
    ))),
    
    div(class = "container mt-5", style = "max-width: 450px;",
        div(class = "card shadow-sm",
            div(class = "card-header text-white text-center fw-bold", style = "background-color: #07234C;", "User Authentication"),
            div(class = "card-body",
                
                navset_underline(
                  id = ns("auth_tabs"),
                  nav_panel("Sign In",
                            div(class = "mt-3",
                                textInput(ns("login_email"), "Email Address", width = "100%"),
                                passwordInput(ns("login_pwd"), "Password", width = "100%"),
                                actionButton(ns("btn_login"), "Secure Log In", class = "btn-primary w-100 mt-2", style = "font-weight: 900;"),
                                
                                # THE FIX: Added Forgot Password Link
                                div(class = "text-center mt-3",
                                    actionLink(ns("lnk_forgot_pwd"), "Forgot email or password?", style = "color: #0D67B8; text-decoration: underline;")
                                )
                            )
                  ),
                  nav_panel("Register",
                            div(class = "mt-3",
                                textInput(ns("reg_fname"), "First Name *", width = "100%"),
                                textInput(ns("reg_lname"), "Last Name *", width = "100%"),
                                selectInput(ns("reg_agency"), "Agency *", choices = c("Loading..." = ""), width = "100%"),
                                textInput(ns("reg_job"), "Job Title", width = "100%"),
                                textInput(ns("reg_phone"), "Phone Number", width = "100%"),
                                hr(),
                                textInput(ns("reg_email"), "Email Address *", width = "100%"),
                                passwordInput(ns("reg_pwd"), "Password *", width = "100%"),
                                p(em("Password must be at least 8 characters and include lowercase, uppercase, numbers, and symbols."), 
                                  style = "font-size: 0.8rem; color: #AA5F40; margin-top: 5px; line-height: 1.2;"),
                                actionButton(ns("btn_register"), "Create Account", class = "btn-success w-100 mt-2", style = "font-weight: 900;")
                            )
                  )
                ),
                div(textOutput(ns("auth_msg")), style = "color: #AA5F40; margin-top: 15px; text-align: center; font-weight: bold;")
            )
        )
    )
  )
}

auth_server <- function(id, db, current_user) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      agencies <- dbGetQuery(db, "SELECT agencyname FROM lkup.agency ORDER BY agencyname")
      updateSelectInput(session, "reg_agency", choices = c("Choose an agency..." = "", agencies$agencyname))
    })
    
    # THE FIX: Expanded supa_auth to handle the "recover" endpoint and made password optional
    supa_auth <- function(email, password = NULL, type = "login") {
      base_url <- Sys.getenv("SUPABASE_URL")
      api_key <- Sys.getenv("SUPABASE_ANON_KEY")
      
      if (type == "login") {
        endpoint <- "/auth/v1/token?grant_type=password"
        body_data <- list(email = email, password = password)
      } else if (type == "signup") {
        endpoint <- "/auth/v1/signup"
        body_data <- list(email = email, password = password)
      } else if (type == "recover") {
        endpoint <- "/auth/v1/recover"
        body_data <- list(email = email)
      }
      
      url <- paste0(base_url, endpoint)
      res <- POST(url, add_headers(apikey = api_key, `Content-Type` = "application/json"), body = toJSON(body_data, auto_unbox = TRUE))
      return(content(res))
    }
    
    # THE FIX: Show Modal when "Forgot Password" is clicked
    observeEvent(input$lnk_forgot_pwd, {
      showModal(modalDialog(
        title = "Reset Password",
        p("Enter your email address below. If we find an account matching that email, we will send you a secure password reset link."),
        textInput(session$ns("reset_email"), "Email Address", width = "100%"),
        footer = tagList(
          tagAppendAttributes(modalButton("Cancel"), style = "color: #333; background-color: #e9ecef; border-color: #ccc;"),
          actionButton(session$ns("btn_send_reset"), "Send Reset Link", class = "btn-primary", style = "font-weight: bold;")
        )
      ))
    })
    
    # THE FIX: Process the Password Reset Request
    observeEvent(input$btn_send_reset, {
      req(input$reset_email)
      reset_em <- trimws(input$reset_email)
      
      if (reset_em == "") {
        showNotification("Please enter an email address.", type = "warning")
        return()
      }
      
      # Step 1: Check Database first to ensure it's a valid registered user
      user_check <- dbGetQuery(db, "SELECT email FROM public.profiles WHERE LOWER(email) = LOWER($1)", params = list(reset_em))
      
      # Step 2: If they exist, ping Supabase to trigger the email
      if (nrow(user_check) > 0) {
        supa_auth(email = reset_em, type = "recover")
      }
      
      # Security Best Practice: Always show the same success message so malicious users can't "guess" emails by seeing different errors
      removeModal()
      showNotification("If that email matches an account in our system, a password reset link has been sent.", type = "message", duration = 8)
    })
    
    
    observeEvent(input$btn_register, {
      if (trimws(input$reg_fname) == "" || trimws(input$reg_lname) == "" || input$reg_agency == "" || trimws(input$reg_email) == "" || trimws(input$reg_pwd) == "") {
        output$auth_msg <- renderText("Error: First Name, Last Name, Agency, Email, and Password are ALL mandatory.")
        return() 
      }
      
      output$auth_msg <- renderText("Creating account...")
      res <- supa_auth(input$reg_email, input$reg_pwd, "signup")
      
      if (!is.null(res[["msg"]]) || !is.null(res[["error_description"]])) {
        err_msg <- if(!is.null(res[["error_description"]])) res[["error_description"]] else res[["msg"]]
        output$auth_msg <- renderText(paste("Error:", err_msg))
      } else {
        new_uuid <- if (!is.null(res[["user"]]) && !is.null(res[["user"]][["id"]])) res[["user"]][["id"]] else res[["id"]]
        
        if (!is.null(new_uuid)) {
          q_upsert <- "
            INSERT INTO public.profiles (id, first_name, last_name, agency, email, job_title, phone) 
            VALUES ($1, $2, $3, $4, $5, $6, $7) 
            ON CONFLICT (id) DO UPDATE 
            SET first_name = EXCLUDED.first_name, last_name = EXCLUDED.last_name, 
                agency = EXCLUDED.agency, email = EXCLUDED.email, 
                job_title = EXCLUDED.job_title, phone = EXCLUDED.phone
          "
          dbExecute(db, q_upsert, params = list(new_uuid, input$reg_fname, input$reg_lname, input$reg_agency, input$reg_email, input$reg_job, input$reg_phone))
          output$auth_msg <- renderText("Account created! Use the Sign in tab to sign in.")
        } else {
          output$auth_msg <- renderText("Error: Account created, but failed to retrieve user ID.")
        }
      }
    })
    #  Pop up the new password box when the URL token is detected
    observeEvent(input$recovery_token, {
      showModal(modalDialog(
        title = "Create New Password",
        p("Welcome back! Please enter your new password below."),
        passwordInput(session$ns("new_pwd"), "New Password", width = "100%"),
        footer = tagList(
          actionButton(session$ns("btn_save_pwd"), "Save Password", class = "btn-success", style = "font-weight: bold;")
        )
      ))
    })
    
   #Securely send the new password to the Supabase Database
    observeEvent(input$btn_save_pwd, {
      req(input$new_pwd)
      
      base_url <- Sys.getenv("SUPABASE_URL")
      api_key <- Sys.getenv("SUPABASE_ANON_KEY")
      url <- paste0(base_url, "/auth/v1/user")
      
      # We use the temporary token from the URL to authorize the password change
      res <- httr::PUT(url, 
                       add_headers(apikey = api_key, 
                                   Authorization = paste("Bearer", input$recovery_token),
                                   `Content-Type` = "application/json"),
                       body = toJSON(list(password = input$new_pwd), auto_unbox = TRUE))
      
      status <- status_code(res)
      
      if (status == 200) {
        removeModal()
        showNotification("Password updated successfully! You may now sign in.", type = "message", duration = 8)
      } else {
        showNotification("Failed to update password. Your reset link may have expired.", type = "error")
      }
    })
    
    observeEvent(input$btn_login, {
      if (is.null(input$login_email) || trimws(input$login_email) == "" || is.null(input$login_pwd) || trimws(input$login_pwd) == "") {
        output$auth_msg <- renderText("Error: Please provide both an email and a password.")
        return()
      }
      
      output$auth_msg <- renderText("Authenticating...")
      res <- supa_auth(input$login_email, input$login_pwd, "login")
      
      if (!is.null(res$access_token)) {
        output$auth_msg <- renderText("Success! Logging you in...")
        user_id <- res$user$id
        prof <- dbGetQuery(db, "SELECT * FROM public.profiles WHERE id = $1", params = list(user_id))
        user_role <- if(nrow(prof) > 0) prof$role[1] else "user"
        
        # If they registered before we added 'email' to profiles, let's patch it quietly in the background
        if (nrow(prof) > 0 && (is.na(prof$email[1]) || prof$email[1] == "")) {
          dbExecute(db, "UPDATE public.profiles SET email = $1 WHERE id = $2", params = list(input$login_email, user_id))
        }
        
        # Calculate exactly when this session should die
        expires_in_seconds <- if (!is.null(res$expires_in)) res$expires_in else 3600
        expiration_time <- Sys.time() + expires_in_seconds
        
        current_user(list(
          email = input$login_email, 
          user_id = user_id, 
          token = res$access_token, 
          role = user_role,
          expires_at = expiration_time # Store the kill switch time
        ))
      } else {
        err_msg <- res$error_description
        if (is.null(err_msg)) err_msg <- res$msg
        if (is.null(err_msg)) err_msg <- res$error
        if (is.null(err_msg)) err_msg <- "Invalid email or password."
        output$auth_msg <- renderText(paste("Error:", err_msg))
      }
    })
  })
}