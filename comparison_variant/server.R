#' ================================================================================================ #
#' Description: R Shiny calculation server
#'
#' Input:
#'
#' Output:
#'
#' Author: Simon Anastasiadis, Akilesh Chokkanathapuram
#'
#' Dependencies: corresponding ui and global files
#'
#' Notes: The use of 'for loops' requires a local environment. See the reference file details/training.
#'
#' Issues:
#'
#' History (reverse order): 
#' 2019 Mar 15 SA first complete prototype, core dashboard functionality complete
#' 2019 Mar 06 AK v0.1 addition of save/load functionality
#' 2019 Feb 25 SA v0
#' ================================================================================================ #

# Define server logic ----
server <- function(input, output, session) {
  
  ## load/save observers --------------------------------------------------------------------------
  #### initial load of saved files ----
  if (identical(list.files(pattern = "\\.RDS$"), character(0))){
    savedSessionFiles <- ""  
  }else{
    savedSessionFiles <- list.files(pattern = "\\.RDS$")
  }
  updateSelectInput(session = session, inputId = "loadFileSelection", choices = savedSessionFiles)
  
  #### load state ----
  observeEvent(input$load_inputs,{
    
    file <- input$loadFileSelection #select the file from the dropdown list box 
    savedInputs <- readRDS(file) #read the input RDS fileinto the system for processing
    inputIDs      <- names(savedInputs) # input ID names
    inputvalues   <- unlist(savedInputs) # input ID values
    
    #running a loop to match the content to the relevant ID's
    for (i in 1:length(inputIDs)) {
      
      #All components except checkboxes are matched here
      session$sendInputMessage(inputIDs[i],  list(value=inputvalues[[i]]))
      
      #checkbox matching begins here
      #Create a manual list of checkbox names
      check_box_names <- lapply(names(journey_description_list),
                                FUN = function(x){ paste0(gsub("[ ,+/\\?()-]","_",x),"_checkbox") })
      
      #dummy dataframe to store values
      check.box.data.frame.loader <- data.frame(matrix(NA, nrow = 1, ncol = length(check_box_names)))
      colnames(check.box.data.frame.loader) <- check_box_names
      
      #identify content from the saved data and segregate it for easier loading
      #ACHOKKANATHAPURAM
      for (i in 1:length(check_box_names)){
        pattern.check <- check_box_names[i]
        check.box.name.choices.checked <- grepl(pattern.check, substr(names(inputvalues), 0, (nchar(names(inputvalues))-1)))
        items.to.load <- which(check.box.name.choices.checked == TRUE)
        loader.buffer <- list(inputvalues[items.to.load])
        loader.buffer <- list(unlist(loader.buffer, use.names = FALSE))
        # print(loader.buffer)
        check.box.data.frame.loader[[i]] <- loader.buffer
      }
    }
    
    #run the segregated content through a loop to load them into the shiny UI
    for(i in 1:ncol(check.box.data.frame.loader)) {
      check.box.to.update <- colnames(check.box.data.frame.loader)[i]
      updateCheckboxGroupInput(
        session = session,
        inputId = check.box.to.update,
        choices = NULL,
        label = NULL,
        selected = unlist(check.box.data.frame.loader[, i], use.names = F)
      )
    }
  })
  
  #### save state ----
  observeEvent(input$save_inputs,{
    saveRDS(reactiveValuesToList(input,all.names = T) , file = paste0(input$StateName, '.RDS'))
    savedSessionFiles <- list.files(pattern = "\\.RDS$")
    updateSelectInput(session = session, inputId = "loadFileSelection", choices = savedSessionFiles)
  })
  
  ## control panels conditional display -----------------------------------------------------------
  #### setup ----
  panel_control <- reactiveValues()
  
  # panel list
  control_panel_logicals <- c("view_group", "view_role", "view_journey", "view_prepost", "view_general")
  group_panel_logicals <- sapply(group_list, function(x){ paste0("view_", gsub("[ ,+/\\?()-]","_",x)) }, USE.NAMES = FALSE)
  
  # setup each conditional panel:
  # 1 - reactive logical for display/not
  # 2 - text output for the panel condition
  # 3 - set textout to always update
  for(panel_logical in c(control_panel_logicals, group_panel_logicals)){
    local({
      p_l <- panel_logical
      
      panel_control[[p_l]] <- FALSE
      output[[p_l]] <- renderText( ifelse(panel_control[[p_l]], "show", "noshow") )
      # set text to always update so it can be used for conditional panel
      outputOptions(output, p_l, suspendWhenHidden = FALSE)
    })
  }
  
  #### observers ----
  observeEvent(input$groupButton,{
    panel_control <- update_logicals(panel_control, toggle = "view_group",
                                     to_false = control_panel_logicals[control_panel_logicals != "view_group"])
  })
  
  observeEvent(input$roleButton,{
    panel_control <- update_logicals(panel_control, toggle = "view_role",
                                     to_false = control_panel_logicals[control_panel_logicals != "view_role"])
  })
  
  observeEvent(input$journeyButton,{
    panel_control <- update_logicals(panel_control, toggle = "view_journey",
                                     to_false = control_panel_logicals[control_panel_logicals != "view_journey"])
  })
  
  observeEvent(input$prepostButton,{
    panel_control <- update_logicals(panel_control, toggle = "view_prepost",
                                     to_false = control_panel_logicals[control_panel_logicals != "view_prepost"])
  })
  
  observeEvent(input$generalButton,{
    panel_control <- update_logicals(panel_control, toggle = "view_general",
                                     to_false = control_panel_logicals[control_panel_logicals != "view_general"])
  })
  
  observeEvent(input$updateButton,{
    panel_control <- update_logicals(panel_control, to_false = control_panel_logicals)
    update_title()
    update_panels()
    update_journey()
    update_pre_post()
    update_general()
  })
  
  ## quick select buttons for journey -------------------------------------------------------------
  #### observers for journey ----
  observeEvent(input$journey_all_button,{
    for(x in names(journey_description_list)){
      tmp_inputID <- paste0("journey_",gsub(" ","_",x),"_checkbox")
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = journey_description_list[[x]])
    }
  })
  
  observeEvent(input$journey_common_button,{
    for(x in names(journey_description_list)){
      tmp_inputID <- paste0("journey_",gsub(" ","_",x),"_checkbox")
      
      selection <- description_controls %>%
        filter(description_display_type == !!enquo(x),
               is_common_journey_element == 1) %>%
        select(description_display_name) %>%
        unlist(use.names = FALSE)
      
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = selection)
    }
  })
  
  observeEvent(input$journey_none_button,{
    for(x in names(journey_description_list)){
      tmp_inputID <- paste0("journey_",gsub(" ","_",x),"_checkbox")
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = character(0))
    }
  })
  
  #### observers for pre & post ----
  observeEvent(input$pre_post_all_button,{
    for(x in names(pre_post_description_list)){
      tmp_inputID <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = pre_post_description_list[[x]])
    }
  })
  
  observeEvent(input$pre_post_common_button,{
    for(x in names(pre_post_description_list)){
      tmp_inputID <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
      
      selection <- description_controls %>%
        filter(description_display_type == !!enquo(x),
               is_common_journey_element == 1) %>%
        select(description_display_name) %>%
        unlist(use.names = FALSE)
      
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = selection)
    }
  })
  
  observeEvent(input$pre_post_none_button,{
    for(x in names(pre_post_description_list)){
      tmp_inputID <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
      updateCheckboxGroupInput(session, tmp_inputID,
                               selected = character(0))
    }
  })

  ## visualisation staging ------------------------------------------------------------------------
  #### setup ----
  output_staging <- reactiveValues()
  output_staging$title <- "No role presently selected"
  
  #### update title ----
  update_title <- function(){
    # get group name
    role_name <- input$role_selectInput
    # update
    output_staging$title <- paste0("Role: ",role_name)
  }
  
  #### update panels ----
  update_panels <- function(){
    for(group in group_list){
      local({
        gg <- group
        panel_control[[paste0("view_", gsub("[ ,+/\\?()-]","_",gg))]] <- gg %in% input$group_checkbox
      })
    }
  }
  
  #### update journey ----
  update_journey <- function(){
    selected_measures <- get_selected_measures(journey_description_list, input,
                                               prefix = "journey_", suffix = "_checkbox")
    # stage journey plots for each role
    for(group in input$group_checkbox){
      local({
        gg <- group
        gg_staging <- paste0(gsub("[ ,+/\\?()-]","_",gg), "_journey")
        output_staging[[gg_staging]] <- plot_timeline(gg, input$role_selectInput, selected_measures)
      })
    }
  }
  
  #### update pre & post figures ----
  update_pre_post <- function(){
    selected_measures <- get_selected_measures(pre_post_description_list, input,
                                               prefix = "pre_post_", suffix = "_checkbox")
    # stage pre/post plots for each role
    for(group in input$group_checkbox){
      local({
        gg <- group
        gg_staging <- paste0(gsub("[ ,+/\\?()-]","_",gg), "_pre_post")
        output_staging[[gg_staging]] <- plot_pre_post(gg, input$role_selectInput, selected_measures)
      })
    }
  }
  
  #### update general figures ----
  update_general <- function(){
    selected_measures <- get_selected_measures(general_description_list, input,
                                               prefix = "general_", suffix = "_checkbox")
    output_staging$general <- plot_general(input$group_checkbox, input$role_selectInput, selected_measures)
  }

  ## output ---------------------------------------------------------------------------------------
  #### output other ----
  output$title <- renderText(output_staging$title)
  
  #### output journey ----
  # renderPlot needed for outputting plot, renderUI needed to make height dynamic
  for(group in group_list){
    local({
      gg <- group
      gg_staging <- paste0(gsub("[ ,+/\\?()-]","_",gg), "_journey")
      gg_plot <- paste0("journey_",gsub("[ ,+/\\?()-]","_",gg))
      gg_ui <- paste0("journey_",gsub("[ ,+/\\?()-]","_",gg),"_ui")
      
      output[[gg_plot]] <- renderPlot( output_staging[[gg_staging]]$figure )
      
      output[[gg_ui]] <- renderUI({ 
        req(output_staging[[gg_staging]]$figure_height)
        plotOutput(gg_plot,height = HEIGHT_PIXELS * (1 + output_staging[[gg_staging]]$figure_height))
      })
    })
  }
  
  #### output pre_post ----
  for(group in group_list){
    local({
      gg <- group
      gg_staging <- paste0(gsub("[ ,+/\\?()-]","_",gg), "_pre_post")
      gg_pre_post_plot <- paste0("pre_post_",gsub("[ ,+/\\?()-]","_",gg))
      gg_pre_post_ui <- paste0("pre_post_",gsub("[ ,+/\\?()-]","_",gg),"_ui")
      
      # plotters for the dynamic number of pre/post plots
      for(ii in 1:MAX_PRE_POST_TYPES){
        local({
          this_i <- ii
          tmp_pre_post <- paste0(gg_pre_post_plot,"_",this_i)
          output[[tmp_pre_post]] <- renderPlot({ 
            req(length(output_staging[[gg_staging]]) >= this_i) # avoids error accessing unavailable subscripts
            output_staging[[gg_staging]][[this_i]] })
        })
      }

      # UI components for the pre/post plots
      output[[gg_pre_post_ui]] <- renderUI({
        req(output_staging[[gg_staging]][[1]])
        
        lapply(1:length(output_staging[[gg_staging]]),
               function(ii){ column(4, plotOutput(paste0(gg_pre_post_plot,"_",ii), height = 300)) })
      })
    })
  }

  #### output general ----
  output$general_ui <- renderUI({
    req(output_staging$general)
    
    for(ii in 1:length(output_staging$general)){
      local({
        # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() 
        # will be the same across all instances, because of when the expression is evaluated.
        this_i <- ii
        tmp_name <- paste0("general_",this_i)
        output[[tmp_name]] <- renderPlot({
          req(length(output_staging$general) >= this_i) # avoids error where attempts to plot more than available
          output_staging$general[[this_i]] }, width = 500, height = 300 )
      })
    }
    
    plot_output_list <- lapply(1:length(output_staging$general), function(x){
      tmp_name <- paste0("general_",x)
      plotOutput(tmp_name, inline = TRUE)
    })
    
    # print(length(output_staging$general))
    # print(length(plot_output_list))
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })
  
  ## other ----------------------------------------------------------------------------------------
  
}