#' ================================================================================================ #
#' Description: R Shiny user interface 
#' 
#' Input: 
#' 
#' Output:
#' 
#' Authors: Simon Anastasiadis, Akilesh Chokkanathapuram 
#' 
#' Dependencies:corresponding server and global files 
#' 
#' Notes: 
#' 
#' Issues: 
#' 
#' History (reverse order):
#' 2019 Mar 15 SA first complete prototype, core dashboard functionality complete
#' 2019 Mar 06 AK v0.1 addition of save/load functionality
#' 2019 Feb 25 SA v0 
#' ================================================================================================ #

## Define UI

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    
    ## tab: visualisation ----
    tabPanel("Visualizations",
             
             br(),
             div( p("DRAFT RESEARCH - IN CONFIDENCE - NOT GOVERNMENT POLICY"), style = "font-size:130%"),
             br(),
             
             
             #### buttons ----
             actionButton("groupButton", "Group"),
             actionButton("roleButton", "Role"),
             actionButton("journeyButton", "Journey"),
             actionButton("prepostButton", "Pre/Post"),
             actionButton("generalButton", "General"),
             actionButton("updateButton", "Update"),
             
             hr(),
             
             #### controls ----
             conditionalPanel(condition = "output.view_group == 'show'",
                              checkboxGroupInput("group_checkbox", h4("Select groups to investigate"),
                                                 choiceNames = group_list, choiceValues = group_list),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_role == 'show'",
                              selectInput("role_selectInput", h4("Select role to display"),
                                          choices = role_list, 
                                          width = "400px", selectize = FALSE, size = 5
                              ),
                              hr()
             ),
             
             
             conditionalPanel(condition = "output.view_journey == 'show'",
                              h4("Select measures to appear on the journey"),
                              
                              actionButton("journey_all_button", "All"),
                              actionButton("journey_common_button", "Common"),
                              actionButton("journey_none_button", "None"),
                              
                              lapply(names(journey_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("journey_",gsub("[ ,+/\\?()-]","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = journey_description_list[[x]],
                                                                 choiceValues = journey_description_list[[x]]))
                                     }),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_prepost == 'show'",
                              h4("Select measures to appear pre & post the journey"),
                              
                              actionButton("pre_post_all_button", "All"),
                              actionButton("pre_post_common_button", "Common"),
                              actionButton("pre_post_none_button", "None"),
                              
                              lapply(names(pre_post_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("pre_post_",gsub("[ ,+/\\?()-]","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = pre_post_description_list[[x]],
                                                                 choiceValues = pre_post_description_list[[x]]))
                                     }),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_general == 'show'",
                              h4("Select the general measures to appear below the journey"),
                              
                              lapply(names(general_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("general_",gsub("[ ,+/\\?()-]","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = general_description_list[[x]],
                                                                 choiceValues = general_description_list[[x]]))
                                     }),
                              hr()
             ),
             
             #### results ----
             div(textOutput("title"), style = "font-size:170%"),
             p("The timeline shows the journey for a representative person. Not every person in the group experiences",
               " every component of the representative journey. The percentages reported for the journey give the",
               " percent of the group who have that component/measure in their journey."),
             p("The timeline plot shows a representative experience for each component for people with the component.",
               " For example, if 'Lab test (10%)' shows 1 event at fortnight 5, then 10% of the people in the",
               " group had a lab test, the average number of fortnights with lab tests is 1, and the representative",
               " timing this occurs in is fortnight 5."),
             hr(),
             
             lapply(group_list, FUN = function(gg){
               
               conditionalPanel(condition = paste0("output.view_",gsub("[ ,+/\\?()-]","_",gg),"== 'show'"), h3(gg),
                                uiOutput(paste0("journey_",gsub("[ ,+/\\?()-]","_",gg),"_ui")),
                                fluidRow(
                                  uiOutput(paste0("pre_post_",gsub("[ ,+/\\?()-]","_",gg),"_ui"))
                                ),
                                hr()
               )
             }),
             
             uiOutput("general_ui"),
             
             #### SIA tool disclaimer ----
             h6('Tool disclaimer'),
             
             div(
               p("This tool has been created to help users visualise and draw insight from data that includes timelines.",
                 " The opinions, findings, insights, recommendations, and conclusions arising from the use of this tool",
                 " are those of the users, not those of the Social Investment Agency (SIA), nor those of the data owners",
                 " or providers."),
               
               p("The tool is licensed under the GNU General Public License v3.0, a copy of which can be found here:",
                 " https://www.gnu.org/licenses/gpl-3.0.html. Any data provided with this tool may have its own license,",
                 " disclaimer, or warranty distinct from that of this tool."),
               style = "font-size:70%"
             ),
             
             #### IDI disclaimer ----
             h6('IDI Disclaimer'),
             
             div(
             p("The results in this data table are not official statistics, they have been created for research",
               " purposes from the Integrated Data Infrastructure (IDI), managed by Statistics New Zealand.",
               "The opinions, findings, recommendations, and conclusions expressed in this [report, paper etc]",
               " are those of the author(s), not Statistics NZ, SIA, or MSD."),
             
             p("Access to the anonymised data used in this study was provided by Statistics NZ in accordance",
               " with security and confidentiality provisions of the Statistics Act 1975. Only people authorised",
               " by the Statistics Act 1975 are allowed to see data about a particular person, household, business,",
               " or organisation, and the results in this data table have been confidentialised to protect these", 
               " groups from identification. Careful consideration has been given to the privacy, security, and ",
               " confidentiality issues associated",
               " with using administrative and survey data in the IDI. Further detail can be found in the Privacy",
               " impact assessment for the Integrated Data Infrastructure available from www.stats.govt.nz."),
             
             p("The results are based in part on tax data supplied by Inland Revenue to Statistics NZ under the",
               " Tax Administration Act 1994. This tax data must be used only for statistical purposes, and no",
               " individual information may be published or disclosed in any other form, or provided to Inland Revenue",
               " for administrative or regulatory purposes. Any person who has had access to the unit record data",
               " has certified that they have been shown, have read, and have understood section 81 of the Tax",
               " Administration Act 1994, which relates to secrecy. Any discussion of data limitations or weaknesses",
               " is in the context of using the IDI for statistical purposes, and is not related to the data's ability",
               " to support Inland Revenue's core operational requirements."),
             style = "font-size:70%")

    ),
    ## tab: bookmarking ----
    tabPanel("Bookmarking",
             br(),
             br(),
             textInput(inputId = "StateName",
                       "Name of the Saved State",
                       "defaultState"),
             
             
             actionButton('save_inputs', 'Save inputs'),
             # actionButton('input_list', 'Get States')
             # useShinyjs(),
             # ## Toggle for showing the list
             # actionButton("toggle.main.button", "Toggle Main"),
             br(),
             conditionalPanel(
               condition = !is.null(list.files(pattern = "\\.RDS$")),
               
               selectInput("loadFileSelection",
                           "Select Session",
                           choices = NULL),
               actionButton('load_inputs', 'Load Selected State')
               
             ))
    ## end of tab wrapper ----
  )
)
