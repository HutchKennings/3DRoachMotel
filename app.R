##########################################
# This program is NOT optimized with     #
# shortened Variable Names.      They    #
# have been, for the most part left as   #
# as long descriptive to aid both me     #
# and others in understanding the logic. #
# There are other obvious improvements   #
# that could be made but since it has    #
# fulfilled its original purpose I will  #
# leave it as is, for now.               #
# If you have questions about the logic  #
# or any other items you may email me    #
# at the email in the referenced paper   #
# in the Shiny Interface - KRH 2026      #
#########################################
### MAXPOWERSFORAPP for ShinyApp on Web = 12
### To limit Server Load
MAXPOWERSFORAPP <- 12 
## YOU MAY CHANGE THIS TO RUN HIGHER POWERS OF 2 
### ON YOUR OWN COMPUTER OR SERVER

library(shiny)
library(shinyjs)
library(plotly)
library(processx)
library(svDialogs)
library(gmp)
library(rstudioapi)
library(rsconnect)

# DO NOT CHANGE ROACH ART BELOW ---- INCLUDING LINEBREAKS
# This includes the ASCII art of Mr. Roach for the Welcome Dialog 
roach_art <- "
<p style='color: blue; font-size: 10px;text-align: center;'>&nbsp;
Mr. Roach
&nbsp;&nbsp;
\"He checks-in but he doesn\'t check-out.\"</p>
<p style='text-align: center;'><img src='RoachMotel.png' width='250'></p>
<p style='color: black; font-size: 10px;text-align: center;'>
An <b>R Shiny App</b> to
plot the paths the roaches take to check-in.
<p style='color: black; font-size: 8px;text-align: center;'>
Based on the paper by Kenneth Roger Hutchings
<br>
\"The Roach Motel with Chutes and Ladders and with Early and Late Check-in\"
<br>
Available at 
<a href='https://' target='_blank' rel='noopener noreferrer'>
<u>SOMEPLACE</u>
</a>
</p>
<p style='color: blue; font-size: 14px;text-align: center;'>Scroll down to enter options and execute program</p>
                       <p></p>
<p style='color: black; font-size: 8px;text-align: right;'>
All concepts and coding Copyright Kenneth Roger Hutchings 2026. 
<br>
\"Roach Motel\" and \"Roaches Check In ... but They Don't Check Out!\" 
<br>
Trademark of United Industries Corporation.
</p>
<p style='color: black; font-size: 14px;text-align: center;'>
Happy Roach Hunting!</p>
"
# DO NOT CHANGE ROACH ART ABOVE ---- INCLUDING LINEBREAKS

#### Needed to define these to prevent errors
RoachCoachList <- c("")
CorR <- "1"
ARN <- 1
ARNzLABEL <- 1
THISTEST <- as.bigz("1")



ui <- fluidPage(
  useShinyjs(),
  withMathJax(), 

# Main Content Row
  fluidRow(
    column(width = 12,
           HTML("<p style='color: blue; font-size: 18px;text-align: center;'> 
                <img src='Vacancy.gif' width='40'>  The Interactive 3-D Roach Motel<br>
                <span style='font-size: 12px;'>A SUBSIDIARY OF HILBERT'S GRAND HOTEL</span> </p>
                <p style='color: blue; font-size: 10px;text-align: center;'>
                Proprietor - Kenneth Roger Hutchings
                </p>
                <p style='color: red; font-size: 8px;text-align: center;'>
                Please wait while the Roach Motel builds. Once done you can drag your mouse to wander around.
                </p>"))
    ),
  fluidRow(
    column(width = 12,
           
           plotlyOutput("plot3D", width = "100%") # Output for the Plotly graph
    )
  ),
  # Button Rows
fluidRow(
  column(width = 12,
         uiOutput("List_of_Roaches"), 
         
         align = "center",
         actionButton("New_Input_button", "Enter New Roaches and/or Options and Re-plot"),
  )
),
fluidRow(
  column(width = 12,
         align = "center",
         shiny::actionButton(
           inputId = 'newTab',
           label = HTML("Open the paper<br> <b>\"The Roach Motel with Chutes and Ladders<br>and with Early and Late Check-in\"<br> - by Kenneth Roger Hutchings </b><br>in a new window"),
           onclick = "window.open('https://posit.co', '_blank')"
         )
  )
),
fluidRow(
  column(width = 12,
         align = "center",
         actionButton("Close_button", "Close Roach Motel App")
  )
)

#End Fluid Page
)


server <- function(input, output, session) {

 # session$onSessionEnded(function() {
#    stopApp()
#  })
  
  observeEvent(input$Close_button, {
    stopApp()
  })


  
  # Changable Variables 
  xyexpansion <- 1.05 # Extends top of X and Y axis, rooms, ceilings, splits etcetera 
  
  # Non Changable Variables 
  LastRoach <- 1
  RoachCoach <- as.array("0") # holds the numbers OF the roaches in the coach
  RoachCoachOccupancy <- length(RoachCoach) # holds the number of roaches in the coach
  lowz <- 0 # Holds the lowest z value reached by any number - used to set z axis 
  highz <- 0 # Holds the highest z value reached by any number - used to set z axis

      
     CorR_modal <- function() {
      modalDialog(
          tags$head(
          tags$style(type="text/css",
                     "label{ display: table-cell;
                     text-align: left; 
                     vertical-align: middle;
                     padding-left: 15px;padding-right: 15px;} 
                     .form-group { display: table-row;}")
        ),
        withMathJax(), 
        h1 (HTML("<p style='color: black; font-size: 16px;text-align: center;'>The Roach Motel</p><p style='color: black; font-size: 8px;text-align: center;'>A SUBSIDIARY OF HILBERT'S GRAND HOTEL</p>")),
        HTML(roach_art),
        hr(),
        radioButtons("CorR_In", HTML("
                     A Coach of Roaches contains Roach Numbers 
                     \\(2^{(x-1)} + 1 \\Longrightarrow 2^{(x)} \\) 
  <br>
                     <span style='font-size: 8px;'>Example: For x = 4, Roaches # 9, 10, 11, 12, 13, 14, 15, & 16.
  <br>                   All roaches in a particular
                     Coach will check-in to the same Assigned Room Number.</span>"),
                     choiceNames = list(
                       "Coach of Roaches",
                       "Single or List of Roaches",
                       "Range of Roaches with Interval"
                     ),
                     choiceValues = list(
                       "1", "0", "2"
                     ),
                     inline = FALSE),
        hr(),

# START Hide and Show inputs based on previous selection
        conditionalPanel(
          condition = "input.CorR_In == 1",
          fluidRow(
            column(12, 
                   textInput("PWR_In", 
                  HTML(paste("Enter the \\(x\\)  in \\(2^{(x)}\\) 
                 <span style='font-size: 8px;'> Limited to \\(x \\le "
                 ,MAXPOWERSFORAPP,
                 "\\) on this Web App to save server load.
                 Will default to \\(x = "
                 ,MAXPOWERSFORAPP,
                 "\\) if \\(x > "
                 ,MAXPOWERSFORAPP,
                 "\\) entered.
                 If you need (or want) to try higher coaches download
                 the code referenced in the paper.</span>  
                  "
                   )),
                   , width = "500px"),
                   hr(),
            )
          )
        ),

        conditionalPanel(
          condition = "input.CorR_In == 0",
          fluidRow(
            column(12, 
                   textInput("RoachList_In", HTML("Enter Roach Number or List<br>
                  <span style='font-size: 8px;'>List must be seperated by a comma or space.</span>"),width = "500px"),
                   hr(),
            )
          )
        ),

        conditionalPanel(
          condition = "input.CorR_In == 2",
          fluidRow(
            column(12, 
                   textInput("Min_In", "First Number in Range",width = "80px"),
                   textInput("Max_In", "Last Number in Range ",width = "80px"),
                   textInput("Interval_In", "Interval between Roaches ",width = "80px"),
                   hr(),
            )
          )
        ),
# END Hide and Show inputs based on previous selection

        
        radioButtons("SHOWPATHS_In", HTML("Show or Hide the Roach Paths.
  <span style='font-size: 8px;'><br>Hiding the paths will only leave the ENTRY POINTS in the Assigned Rooms.  This can be useful to see the pattern of Entry Doors to Assigned Room.</span>"),
                     choiceNames = list(
                       "Show Roach Pathes",
                       "Hide Roach Pathes"
                     ),
                     choiceValues = list(
                       "1", "0"
                     ),
                     inline = FALSE),
        hr(),
        radioButtons("SHOWSPLIT_In", HTML("
                     Show or Hide the Split between the left and right sides of the Motel.
                    <span style='font-size: 8px;'> Note that showing the split can help the view is some cases 
                     and harm it in other cases depending on angle of view and 
                     the number of roaches. Recommend NO SPLIT when No Hallway Effect.</span>"),
                     choiceNames = list(
                       "Show Split",
                       "Hide Split"
                     ),
                     choiceValues = list(
                       "1", "0"
                     ),
                     inline = FALSE),
        hr(),
        textInput("HallwayEffect_In", HTML("Set Hallway Effect 
        <span style='font-size: 8px;'>(Default Hallway Effect is 2) <br> Increase/Decrease this integer value for more/less Hallway Effect. 
        To remove Hallway effect set value to 0. 
        <br>Remember - 
                  The Hallway Effect is an artificial effect for visual 
                  assistance only.</span>"),width = "80px",value = "2"),
        hr(),
        radioButtons("SHADE_ARN_In", HTML("<p>Assigned Room Number (ARN) for any Roach is:</p> 
        $$ ARN = \\lceil \\log_2(\\text{Roach Number}) \\rceil  $$
<p style='font-size: 8px;'>Coaches of roaches all have the same assigned room.
      For multiple roaches, the SHADED assigned room will be for
      the LOWEST roach.</p>"),
                     choiceNames = list(
                       "Shade Assigned Room",
                       "Don't Shade Assigned Room"
                     ),
                     choiceValues = list(
                       "1", "0"
                     ),
                     inline = FALSE),
        hr(),
        radioButtons("SHOW_TERMLINES_In", 
                     HTML("Show the Infinity Lines after the Roach is in it's Assigned Room?                                  
            <span style='font-size: 8px;'><br>After a Roach reaches it's Assigned Room it will stay there
        for infinity. You can choose to show this infinite line or not. 
      It can clutter the plot when Roaches are not in the same Coach.</span>"),
                     choiceNames = list(
                       "Hide Infinity Lines",
                       "Show Infinity Lines"
                     ),
                     choiceValues = list(
                       "0", "1"
                     ),
                     inline = FALSE),
        hr(),
        radioButtons("SHOW_ROACHIDENTITY_In", 
                     HTML(
                  "After a Roach reaches it's 
                       Assigned Room, do you want it's 
                       identity integer plotted above it? 
                       <span style='font-size: 8px;'><br>This is useful but can clutter the plot when the quantity
                       of roaches is high.</span>
                       "
        ),
        choiceNames = list(
          "Show Roach Identities",
          "Hide Roach Identities"
        ),
        choiceValues = list(
          "1", "0"
        ),
        inline = FALSE),
       hr(),
       radioButtons("SHOWCEILINGS_In", 
                    HTML(
                      "Do you want to show all the room ceilings?
                      <span style='font-size: 8px;'><br>
                      This may make it hard to view at different angles but can help to visualize the structure of the motel.  Use judiciously.</span>"
                    ),
       choiceNames = list(
         "Hide All Room Ceilings",
         "Show All Room Ceilings"
       ),
       choiceValues = list(
         "0", "1"
       ),
       inline = FALSE),

        footer = tagList(
          modalButton("Cancel"),
          actionButton("CorR_In_ok", HTML("Confirm Entries and Plot Paths<br> <span style='color: red;'>BE PATIENT!!</span> - Roaches will get there!"))
        ),
        easyClose = FALSE # Requires user to use buttons, not click outside
      )
    }
    # Show modal 
    showModal(CorR_modal())
    ## HIDE ROW BELOW
    observeEvent(input$toggle_row, {
      if (input$toggle_row) {
        show("additional_row")
      } else {
        hide("additional_row")
      }
    })
    ## HIDE ROW ABOVE
    observeEvent(input$CorR_In_ok, {

      # Remove the modal dialog once input is processed
      removeModal()
    })
    
    ## DO IT AGAIN
      observeEvent(input$New_Input_button, {
        session$reload()
        })
    ## END DO IT AGAIN
        Plot_it <- eventReactive(input$CorR_In_ok, {
          # All server logic goes here
      # Define modal dialog for getting CorR
      
      
        # LOAD THE ROACH COACH
    if(input$CorR_In==1){
      #### NORMAL COACH OF ROACHES
      ISGOOD <- 0
      CountPowers <- 1
      while(ISGOOD == 0){
        if(input$PWR_In == CountPowers){
          PWR <- CountPowers
          ISGOOD <- 1
        }
        if(MAXPOWERSFORAPP== CountPowers){
          PWR <- CountPowers
          ISGOOD <- 1
        }
        CountPowers <-  CountPowers + 1
      }  
      TESTNUMBER <- 2^(as.numeric(PWR)-1)+1
      ARN <- ceiling(log.bigz(as.bigz(TESTNUMBER),2)) # Assigned Room Number
      ARNzLABEL <- ARN # Save for use in graphics and labeling
      MAXTEST <- 2^(as.numeric(PWR))
      loadroachcoach <- 1
      while(TESTNUMBER <= MAXTEST){
        RoachCoach[loadroachcoach] <- TESTNUMBER
        TESTNUMBER <- TESTNUMBER + 1
        loadroachcoach <- loadroachcoach + 1
      }
    }else if(input$CorR_In==2){
      #### RANGE OF ROACHES
      BOTTOM_RANGE <- as.bigz("3") # NEVER 1 --> RUNS FOREVER
      TOP_RANGE <- as.bigz("16384") # NEVER 1 --> RUNS FOREVER
      RANGE_INTERVAL <- as.bigz("16384") # Identical paths for ??
      # GET RANGES AND INTERVAL
      # BOTTOM RANGE
#      BOTTOM_RANGE <- showPrompt("First Roach?", "Enter Enter number of first roach.", default = as.character(BOTTOM_RANGE))
 #     if(BOTTOM_RANGE >= "1"){
  #    }else{
   #     BOTTOM_RANGE <- showPrompt("First Roach?", "Enter Enter number of first roach.", default = as.character(BOTTOM_RANGE))
  #    }
      # TOP RANGE
  #    TOP_RANGE <- showPrompt("Last Roach?", "Enter Enter number of possible last roach. Whether it is actually the last roach depends on the interval selected in the next step.", default = as.character(TOP_RANGE))
  #    if(TOP_RANGE >= "1"){
  #    }else{
  #      TOP_RANGE <- showPrompt("Last Roach?", "Enter Enter number of possible last roach. Whether it is actually the last roach depends on the interval selected in the next step.", default = as.character(TOP_RANGE))
  #    }
      # INTERVAL
  #    RANGE_INTERVAL <- showPrompt("Range Interval?", "Enter an interval that you want to see between Roaches.  Powers of 2^x will provide identical paths up to Step x.  Any interval greater than or equal to 1 may be entered.", default = as.character(RANGE_INTERVAL))
  #    if(RANGE_INTERVAL >= "1"){
  #    }else{
  #      RANGE_INTERVAL <- showPrompt("Range Interval?", "Enter an interval that you want to see between Roaches.  Powers of 2^x will provide identical paths up to Step x.  Any interval greater than or equal to 1 may be entered.", default = as.character(RANGE_INTERVAL))
  #    }
      BOTTOM_RANGE <- as.bigz(input$Min_In) 
      TOP_RANGE <- as.bigz(input$Max_In) 
      RANGE_INTERVAL <- as.bigz(input$Interval_In) 
      loadroachcoach <- 1
  #    BOTTOM_RANGE
  #    TOP_RANGE
      while(BOTTOM_RANGE <= TOP_RANGE){
        RoachCoach[loadroachcoach] <- as.character(BOTTOM_RANGE)
        BOTTOM_RANGE <- add.bigz(BOTTOM_RANGE,RANGE_INTERVAL)
        loadroachcoach <- loadroachcoach + 1
      }
      # Assigned Room Number BASED ON LOWEST NUMBER IN LIST
      ARN <- ceiling(log.bigz(as.bigz(min(RoachCoach)),2)) 
      PWR <- ceiling(log.bigz(as.bigz(max(RoachCoach)),2)) # Needed for sizing Roach and Door labels
      PWR <- 0 # Needed for sizing Roach and Door labels
      ARNzLABEL <- ARN # Save for use in graphics and labeling since ARN may be different for each Roach in Coach
    }else{
      #### LIST OF ROACHES
  

#      RoachCoach <- showPrompt("Enter the list of Roaches here (Single or List)","Enter the Number of the Roaches you want to plot, seperated by commas or spaces.  You can paste a list of numbers that meets the seperation criteria in.", default = "0")
#      RoachCoach <- strsplit(RoachCoach, split = ",+")
      RoachCoach <- strsplit(input$RoachList_In, split = ",+")
      RoachCoach <- unlist(RoachCoach)
      RoachCoach <- strsplit(RoachCoach, split ="\\s+")
      RoachCoach <- unlist(RoachCoach)
      RoachCoach <- strsplit(RoachCoach, split ="NA") # For inadvertent double space and commas
      RoachCoach <- unlist(RoachCoach)
      ARN <- ceiling(log.bigz(as.bigz(min(RoachCoach)),2)) 
      PWR <- ceiling(log.bigz(as.bigz(max(RoachCoach)),2)) # Needed for sizing Roach and Door labels
      PWR <- 0 # Needed for sizing Roach and Door labels
      ARNzLABEL <- ARN # Save for use in graphics and labeling since ARN may be different for each Roach in Coach
    }
    
    RoachCoachOccupancy <- length(RoachCoach)

    if(RoachCoachOccupancy>2^MAXPOWERSFORAPP){
      RoachCoachOccupancy <- 2^MAXPOWERSFORAPP
    }
    
    HALLWAYADJUST <- as.numeric(input$HallwayEffect_In)  

    
    # END OF DIALOGS AND INPUTS 
    ##########################
    ##########################
    # START RUN LOOP # 1 FOR ALL ROACHS IN COACH 
    # TO find HIGHEST STEP  and MAX x and y axis FOR PLOT
    # Note : Must run loop here and then again later for actual plot
    step <- 1
    maxsteps <- 1
    highestx <- 1
    RoachRoleCall <- 1
    while(RoachRoleCall <= RoachCoachOccupancy){
      step <- 1
      countx <- 1
      zF <- 0 # To determine lowest z and highest z
      THISTEST <- as.bigz(RoachCoach[RoachRoleCall])
      thisone <- THISTEST
      while(thisone > "1"){
        # IF EVEN
        if(mod.bigz(thisone,"2") == "0"){
          thisone = div.bigz(thisone,"2")
          zF <- zF + 1
          if(zF < lowz){lowz <- floor(zF)}
          if(zF > highz-1){highz <- ceiling(zF)}
          # IF ODD
        }else{
          thisone = div.bigz(add.bigz(mul.bigz(thisone,"3"),"1"),"2")
          zF <- zF - log2(1.5)
          if(zF < lowz){lowz <- floor(zF)}
          if(zF > highz-1){highz <- ceiling(zF)}
          countx <- countx + 1
          if(highestx<countx){
            highestx<-countx
            LastRoach <- as.character(THISTEST)
          }
        }  
        step <- step + 1 
        if(zF < lowz){lowz <- floor(zF)}
        if(zF > highz-1){highz <- ceiling(zF)}
        if(maxsteps > step){}else{maxsteps <- step}
      }
      RoachRoleCall <- RoachRoleCall + 1
    }
    # END RUN LOOP # 1 FOR ALL ROACHS IN COACH 
    # TO find HIGHEST STEP  and MAX x and y axis FOR PLOT
    ##########################
    
    
    
    
    
    
    
    
    ##########################
    # START BUILD THE MOTEL - prior to plotting Roaches
    
    # Set Plot Axis
    axx <- list(range = c(-2, (highestx + HALLWAYADJUST)*xyexpansion), title = "Right Side")
    axy <- list(range = c(-2, (highestx + HALLWAYADJUST)*xyexpansion), title = "Left Side")
    axz <- list(range = c(lowz, highz + 2), title = "Rooms & Basements")
    
    #Defining data frames for ASSIGNED ROOM and CEILINGS
    #ASSIGNED ROOM SIDE COORDINATES
    meshmax <- (highestx + HALLWAYADJUST) * xyexpansion
    eoARN <- ARN%%2  # Determine if ARN is on Left or Right Side
    if(eoARN ==1){
      df_mesh <- data.frame(X_VAL = c(0, 0, meshmax, meshmax, 0, 0, meshmax, meshmax),
                            Y_VAL = c(0, 0, 0, meshmax, 0, 0, 0, meshmax),
                            Z_VAL = c(ARN-1, ARN-1, ARN-1, ARN-1, ARN+1, ARN+1, ARN+1, ARN+1))
    }else{
      df_mesh <- data.frame(
        X_VAL = c(0, 0, 0, meshmax, 0, 0, 0, meshmax),
        Y_VAL = c(0, 0, meshmax, meshmax, 0, 0, meshmax, meshmax),
        Z_VAL = c(ARN-1, ARN-1, ARN-1, ARN-1, ARN+1, ARN+1, ARN+1, ARN+1))
    }
    #OPPOSITE ROOM SIDE COORDINATES
    if(eoARN ==0){
      df_Opposite <- data.frame(X_VAL = c(0, 0, meshmax, meshmax, 0, 0, meshmax, meshmax),
                                Y_VAL = c(0, 0, 0, meshmax, 0, 0, 0, meshmax),
                                Z_VAL = c(ARN-2, ARN-2, ARN-2, ARN-2, ARN, ARN, ARN, ARN))
    }else{
      df_Opposite <- data.frame(
        X_VAL = c(0, 0, 0, meshmax, 0, 0, 0, meshmax),
        Y_VAL = c(0, 0, meshmax, meshmax, 0, 0, meshmax, meshmax),
        Z_VAL = c(ARN-2, ARN-2, ARN-2, ARN-2, ARN, ARN, ARN, ARN))
    }
    
    
    #HALLWAY SPLIT LINE
    if(input$SHOWSPLIT_In == 1){
      #Defining data frame for mesh
      meshmax <- (maxsteps + HALLWAYADJUST) * xyexpansion
      df_hallwaysplit <- data.frame(X_VAL = c(0, meshmax, 0, meshmax, 0, meshmax, 0, meshmax),
                                    Y_VAL = c(0, meshmax, 0, meshmax, 0, meshmax, 0, meshmax),
                                    Z_VAL = c(lowz, lowz, lowz, lowz, highz+1, highz+1, highz+1, highz+1))
    }
    
    #Set Margins for Plot
    m <- list(
      l = 1, # left margin
      r = 1, # right margin
      b = 1, # bottom margin
      t = 1, # top margin
      #    t = "",
      pad = 6 # padding between plot area and axis titles
    )
    
    ## Make Empty Plot with layout
    themotel <- plot_ly()%>%
      layout(margin = m,
             title=NULL,
             scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
                          camera = 
                            list(eye = list(x = -2.1, y = -1.3, z = 0),
                                 #list(eye = list(x = -1.9, y = -1., z = 1.0),
                                 projection = "perspective"
                            )),
             showlegend = FALSE
      )
    
    # ADD Shaded Assigned Room to The Motel if selected
    if(input$SHADE_ARN_In == 1){
      themotel <- themotel %>% add_trace(type = 'mesh3d',
                                         data = df_mesh,
                                         x = ~X_VAL,
                                         y = ~Y_VAL,
                                         z = ~Z_VAL,
                                         i = c(7, 0, 0, 0, 4, 4, 6, 1, 4, 0, 3, 6),
                                         j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                                         k = c(0, 7, 2, 3, 6, 7, 1, 6, 5, 5, 7, 2),
                                         facecolor = rep("black", 12),
                                         opacity = 0.1
      )
    }
    
    # ADD Split to The Motel if selected
    if(input$SHOWSPLIT_In == 1){
      themotel <- themotel %>% add_trace(type = 'mesh3d',
                                         data = df_hallwaysplit,
                                         x = ~X_VAL,
                                         y = ~Y_VAL,
                                         z = ~Z_VAL,
                                         i = c(7, 0, 0, 0, 4, 4, 6, 1, 4, 0, 3, 6),
                                         j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                                         k = c(0, 7, 2, 3, 6, 7, 1, 6, 5, 5, 7, 2),
                                         facecolor = rep("black", 12),
                                         opacity = 0.1
      )
    }
    
    
    # ADD DOOR
    
    themotel <- themotel %>% 
      add_trace(
        x = c(0,-2),
        y = c(0,-2),
        z = c(0,-1),
        type = 'scatter3d',
        mode = "lines",
        line = list(
          #      color = 'black',
          color = "black",
          width = 5)
      )
    
    themotel <- themotel %>% add_trace(
      x = -2, 
      y = -2, 
      z = -2.5, 
      type = "scatter3d", 
      mode = "text", 
      text = "DOOR", 
      textfont = list(color = '#black',
                      family = 'sans serif',
                      size = 10 - as.numeric(PWR)/1.5)
    )
    
    themotel <- themotel %>% add_trace(
      x = 0, 
      y = 0, 
      z = 0, 
      type = "scatter3d", 
      mode = "markers", 
      marker = list(
        color = 'black',
        #  color = 'green', 
        size = 3)       
    )
    
    ### ADD OTHER ALTERNATING ROOMS IF DESIRED
    
    startrooms <- lowz
    endrooms <- highz
    
    if(input$SHADE_ARN_In ==1){
      if(ARNzLABEL%%2==0){
        themotel <- themotel %>% add_trace(
          x = -2, 
          y = -2, 
          #    x = highestx, 
          #    y = 0, 
          #z = ARNzLABEL-1, 
          z = ARNzLABEL -.5,
          type = "scatter3d", 
          mode = "text", 
          text = paste("ARN ",ARNzLABEL,sep=""), 
          textfont = list(color = '#black',
                          family = 'sans serif',
                          size = 10 - as.numeric(PWR)/1.5)
        )
      }else{
        themotel <- themotel %>% add_trace(
          x = -2, 
          y = -2, 
          #    x = highestx, 
          #    y = 0, 
          #z = ARNzLABEL-1, 
          z = ARNzLABEL -.5, 
          type = "scatter3d", 
          mode = "text", 
          text = paste("ARN ",ARNzLABEL,sep=""), 
          textfont = list(color = '#black',
                          family = 'sans serif',
                          size = 10 - as.numeric(PWR)/1.5)
        )
      }
    }
    
    
    if(input$SHOWCEILINGS_In == 1){
      while(startrooms<=endrooms){
        if(ARNzLABEL%%2==startrooms%%2){
          themotel <- themotel %>% add_trace(type = 'mesh3d',
                                             data = df_mesh,
                                             x = ~X_VAL,
                                             y = ~Y_VAL,
                                             #                       z = ~Z_VAL,
                                             z = c(startrooms+1, startrooms+1, startrooms+1, startrooms+1, startrooms +1, startrooms +1, startrooms +1, startrooms +1),                             
                                             i = c(7, 0, 0, 0, 4, 4, 6, 1, 4, 0, 3, 6),
                                             j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                                             k = c(0, 7, 2, 3, 6, 7, 1, 6, 5, 5, 7, 2),
                                             facecolor = rep("black", 12),
                                             opacity = 0.1)
        }else{
          themotel <- themotel %>% add_trace(type = 'mesh3d',
                                             data = df_Opposite,
                                             x = ~X_VAL,
                                             y = ~Y_VAL,
                                             #                             z = ~Z_VAL,
                                             z = c(startrooms+1, startrooms+1, startrooms+1, startrooms+1, startrooms +1, startrooms +1, startrooms +1, startrooms +1),                             
                                             i = c(7, 0, 0, 0, 4, 4, 6, 1, 4, 0, 3, 6),
                                             j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                                             k = c(0, 7, 2, 3, 6, 7, 1, 6, 5, 5, 7, 2),
                                             facecolor = rep("black", 12),
                                             opacity = 0.1)
        }
        startrooms <- startrooms + 1
      }
      
    }
    
    ## Make Icon Bar always visible
    themotel <- themotel %>% config(displayModeBar = TRUE)
    
    # END BUILD THE MOTEL - prior to plotting Roaches
    ##########################
    
    
    ### THIS IS WHERE THE ACTUAL ROACH PATHS ARE DONE
    #LadderAndChute Matrix - Keeps track of Ladders and Chutes AT each step for each roach
    LC_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps+1)
    #Room Number Matrix - Keeps track of Room after each step for each roach
    RN_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps+1)
    #The Number Matrix - Keeps track of Roaches Value after each step for each roach
    TN_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps+1)
    #The Z Matrix - Keeps track of Z Value after each step for each roach
    z_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps + 1)
    #The X Matrix - Keeps track of X Value after each step for each roach
    x_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps + 1)
    #The Y Matrix - Keeps track of Y Value after each step for each roach
    y_matrix <- matrix(c(0), nrow = RoachCoachOccupancy + 1, ncol = maxsteps + 1)
    #The Solved at X Matrix - Keeps track of whether any Roach has previously entered it's ARN at that value of X
    solveatx <- matrix(c(0), nrow = 1, ncol = maxsteps + 1)
    #The Solved at Y Matrix - Keeps track of whether any Roach has previously entered it's ARN at that value of Y
    solveaty <- matrix(c(0), nrow = 1, ncol = maxsteps + 1)
    # BUMPUP matrix provides for keeping track of how many have solved to bump up the Number of the Roach in the plot so no overlap
    BUMPUP <- matrix(c(-.2), nrow = 1, ncol = maxsteps + 1)
    step <- 1
    matrixrow <- 0
    RoachRoleCall <- 1
    while(RoachRoleCall <= RoachCoachOccupancy){
      matrixrow <- matrixrow + 1
      step <- 1
      THISTEST <- as.bigz(RoachCoach[RoachRoleCall])
      thisone <- THISTEST
      TI <- as.bigz(THISTEST)  #Test Roach's Number
      LC <- as.array(0) # Store Ladders and Chutes
      ARN <- ceiling(log.bigz(TI,2)) # Assigned Room Number for this Roach
      TN <- as.array(as.numeric(as.character(TI))) # Store Value after Step
      TN_matrix[matrixrow,1] <- as.numeric(as.character(thisone))
      while(thisone > "1"){
        # IF EVEN
        if(mod.bigz(thisone,"2") == "0"){
          thisone = div.bigz(thisone,"2")
          LC[step] <- 0
          LC_matrix[matrixrow,step] <- 0
          TN[step + 1] <- as.numeric(as.character(thisone))
          TN_matrix[matrixrow,step+1] <- as.numeric(as.character(thisone))
          # IF ODD
        }else{
          thisone = div.bigz(add.bigz(mul.bigz(thisone,"3"),"1"),"2")
          LC[step] <- 1  
          LC_matrix[matrixrow,step] <- 1
          TN[step + 1] <- as.numeric(as.character(thisone))
          TN_matrix[matrixrow,step+1] <- as.numeric(as.character(thisone))
        }  
        step <- step + 1 
      }  
      
      #Get Z,RN,X,Y
      z <- as.array(0)
      RN <- as.array(0) # Room Number
      x <- as.array(0)
      y <- as.array(0)
      
      # Find Z
      nextsteps <- step
      step <- 1
      while(step<nextsteps){
        if(LC_matrix[matrixrow,step] == 0){
          z[step+1] <- z[step] + 1
          z_matrix[matrixrow,step+1] <- z_matrix[matrixrow,step] + 1
        }else{
          z[step+1] <- z[step] - log2(1.5)
          z_matrix[matrixrow,step+1] <- z_matrix[matrixrow,step]  - log2(1.5)
        }
        step <- step + 1
      }
      while (step <= maxsteps){
        z_matrix[matrixrow,step+1] <- z_matrix[matrixrow,step]
        step <- step + 1
      }
      
      
      # GET RoomNumber RN
      nextsteps <- step
      step <- 1
      while(step<nextsteps){
        if(RN_matrix[matrixrow,step] == ARN){
          RN[step+1] <- RN[step]
          RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step]
        }else if(LC_matrix[matrixrow,step] == 0){
          RN[step+1] <- RN[step] + 1
          RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step] + 1
        }else if(LC_matrix[matrixrow,step]== 1){
          if(z_matrix[matrixrow,step+1]<=RN_matrix[matrixrow,step]-1){
            RN[step+1] <- RN[step] - 1
            RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step] - 1
          }else if(step>1){
            if(RN_matrix[matrixrow,step-1] == RN_matrix[matrixrow,step]){
              RN[step+1] <- RN[step] - 1
              RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step] - 1
            } else {
              RN[step+1] <- RN[step]
              RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step]
            }
          }else{
            if(z_matrix[matrixrow,step+1]>RN_matrix[matrixrow,step]-1){
              RN[step+1] <- RN[step]
              RN_matrix[matrixrow,step+1] <- RN_matrix[matrixrow,step]
            }
          }
        }
        step <- step + 1
      }
      
      
      
      # GET X and Y
      nextsteps <- step
      step <- 1
      while(step<nextsteps){
        if(LC_matrix[matrixrow,step] == 0){
          x[step + 1] <- x[step]
          x_matrix[matrixrow,step+1] <- x_matrix[matrixrow,step]
          y[step + 1] <- y[step]
          y_matrix[matrixrow,step+1] <- y_matrix[matrixrow,step]
        }else{
          x[step + 1] <- x[step] + 1
          x_matrix[matrixrow,step+1] <- x_matrix[matrixrow,step] + 1
          y[step + 1] <- y[step] + 1
          y_matrix[matrixrow,step+1] <- y_matrix[matrixrow,step] + 1
        }
        step <- step + 1
      }
      # Fill rest of Matrix Row
      while (step <= maxsteps){
        x_matrix[matrixrow,step+1] <- x_matrix[matrixrow,step]
        y_matrix[matrixrow,step+1] <- y_matrix[matrixrow,step]
        step <- step + 1
      }
      
      
      
      # ADJUST X and Y
      nextsteps <- step
      step <- 2
      while(step<=nextsteps){
        if(RN_matrix[matrixrow,step]%%2 == 0){
          x[step] <- x[step] 
          x_matrix[matrixrow,step] <- x_matrix[matrixrow,step]
          y[step] <- y[step] + HALLWAYADJUST
          y_matrix[matrixrow,step] <- y_matrix[matrixrow,step] + HALLWAYADJUST
        }else{
          x[step] <- x[step] + HALLWAYADJUST
          x_matrix[matrixrow,step] <- x_matrix[matrixrow,step] + HALLWAYADJUST
          y[step] <- y[step] 
          y_matrix[matrixrow,step] <- y_matrix[matrixrow,step]
        }
        step <- step + 1
      }
      
      # Get one row of data for the paths from the matrix
      x_one_row_df <- c(x_matrix[matrixrow, , drop = FALSE])
      y_one_row_df <- c(y_matrix[matrixrow, , drop = FALSE])
      z_one_row_df <- c(z_matrix[matrixrow, , drop = FALSE])
      
      df_thisn <- data.frame(
        X_THIS = x_one_row_df,
        Y_THIS = y_one_row_df,
        Z_THIS = z_one_row_df
      )
      
      if(input$SHOWPATHS_In==1){
        themotel <- themotel %>% 
          add_trace(
            data = df_thisn, 
            x = ~X_THIS,
            y = ~Y_THIS,
            z = ~Z_THIS,
            type = 'scatter3d',
            mode = "markers+lines",
            marker = list(
              color = 'black',
              size = 1),          
            line = list(
              color = 'black',
              width = 1)
          )
      }
      
      #TERMINATION LINE
      if(input$SHOW_TERMLINES_In==1){
        if(ceiling(log.bigz(THISTEST,2))%%2==0){
          df_termline <- data.frame(
            X_THIS = c(x_one_row_df[maxsteps+1],x_one_row_df[maxsteps+1]),
            Y_THIS = c(y_one_row_df[maxsteps+1],maxsteps),
            Z_THIS = c(z_one_row_df[maxsteps+1],z_one_row_df[maxsteps+1])
          )
        }else{
          df_termline <- data.frame(
            X_THIS = c(x_one_row_df[maxsteps+1],maxsteps),
            Y_THIS = c(y_one_row_df[maxsteps+1],y_one_row_df[maxsteps+1]),
            Z_THIS = c(z_one_row_df[maxsteps+1],z_one_row_df[maxsteps+1])
          )
        }
        # Plot the termination line
        themotel <- themotel %>% 
          add_trace(
            data = df_termline, 
            x = ~X_THIS,
            y = ~Y_THIS,
            z = ~Z_THIS,
            type = 'scatter3d',
            mode = "markers+lines",
            marker = list(
              color = 'black',
              size = 2),          
            line = list(
              color = 'black',
              width = 1)
          )
      }else{
        if(ceiling(log.bigz(THISTEST,2))%%2==0){
          df_termline <- data.frame(
            X_THIS = c(x_one_row_df[maxsteps+1],x_one_row_df[maxsteps+1]),
            Y_THIS = c(y_one_row_df[maxsteps+1],maxsteps),
            Z_THIS = c(z_one_row_df[maxsteps+1],z_one_row_df[maxsteps+1])
          )
        }else{
          df_termline <- data.frame(
            X_THIS = c(x_one_row_df[maxsteps+1],maxsteps),
            Y_THIS = c(y_one_row_df[maxsteps+1],y_one_row_df[maxsteps+1]),
            Z_THIS = c(z_one_row_df[maxsteps+1],z_one_row_df[maxsteps+1])
          )
        }
        # Create the 3D line plot
        themotel <- themotel %>% 
          add_trace(
            data = df_termline, 
            x = ~X_THIS,
            y = ~Y_THIS,
            z = ~Z_THIS,
            type = 'scatter3d',
            mode = "markers",
            marker = list(
              color = 'black',
              size = 2)
          )
      }
      
      #BUMP UP ANNOTATIONS IF MORE THAN ONE AT POINT
      if(x_one_row_df[maxsteps+1]==0){
        if(y_one_row_df[maxsteps+1]==0){
          thisBUMP <- .5
        }
      }else{
        if(solveatx[x_one_row_df[maxsteps+1]] > 0){
          BUMPUP[x_one_row_df[maxsteps+1]] <- BUMPUP[x_one_row_df[maxsteps+1]] + .5
          thisBUMP <- BUMPUP[x_one_row_df[maxsteps+1]]
        }else{
          BUMPUP[x_one_row_df[maxsteps+1]] <- .5
          thisBUMP <- BUMPUP[x_one_row_df[maxsteps+1]]
        }
      }
      
      if(y_one_row_df[maxsteps+1]==0){
        if(x_one_row_df[maxsteps+1]==0){
          thisBUMP <- .5
        }
      }else{
        if(solveaty[y_one_row_df[maxsteps+1]] > 0){
          BUMPUP[y_one_row_df[maxsteps+1]] <- BUMPUP[y_one_row_df[maxsteps+1]] + .5
          thisBUMP <- BUMPUP[y_one_row_df[maxsteps+1]]
        }else{
          BUMPUP[y_one_row_df[maxsteps+1]] <- .5
          thisBUMP <- BUMPUP[y_one_row_df[maxsteps+1]]
        }
      }
      
      ## STORE IF ONE HAS SOLVED AT THAT POINT AT LEAST ONCE
      solveatx[x_matrix[matrixrow,maxsteps+1]]<- x_matrix[matrixrow,maxsteps+1]
      solveaty[y_matrix[matrixrow,maxsteps+1]]<- y_matrix[matrixrow,maxsteps+1]
      
      if(input$SHOW_ROACHIDENTITY_In==1){
        themotel <- themotel %>% add_trace(
          x = x_one_row_df[maxsteps+1], 
          y = y_one_row_df[maxsteps+1], 
          z = z_one_row_df[maxsteps+1] + thisBUMP, 
          type = "scatter3d", 
          mode = "text", 
          text = as.character(THISTEST), 
          textfont = list(color = 'black',
                          family = 'sans serif',
                          size = 10 - as.numeric(PWR)/1.5)
        )
      }  
      
      ### Adjust Z axis Ticks and Labels
      if(round((highz + 2 - lowz)*.05)%%2 == 1){
        zdiff <- round((highz + 2 - lowz)*.05)
      }else{
        zdiff <- round((highz + 2 - lowz)*.05) + 1
      }
      zcount <- 1
      zticklist <- lowz
      ztickvals <- as.array(0)
      zticktext <- as.array("")
      while(zticklist < highz + 2){
        ztickvals[zcount] <- zticklist
        if(zticklist%%2 == ARNzLABEL%%2){
          zticktext[zcount] <- paste("",zticklist,sep="")
        }else{
          zticktext[zcount] <- paste("",sep="")
        }
        zticklist <- zticklist + zdiff
        #zticklist <- zticklist + 1
        #  zcount <- zcount + 1
        zcount <- zcount + 1
      }
      ax_z_single_tick <- list(
        tickmode = "array", # Set tick mode to array to use custom values
        tickvals = ~ztickvals,   # Specify the single value for the tick
        ticktext = ~zticktext # Optional: provide a custom label
      )
      
      # Update the layout to apply the custom z-axis setting
      themotel <- themotel %>% layout(scene = list(zaxis = ax_z_single_tick))
      
      RoachRoleCall <- RoachRoleCall +1    
      print(as.character(THISTEST)) # Print to CONSOLE to track progress
    }
    
    
    
    RoachCoachList <- RoachCoach[1]
    RoachRoleCall <- 2
    while(RoachRoleCall <= RoachCoachOccupancy){
      RoachCoachList <- paste(RoachCoachList, RoachCoach[RoachRoleCall], sep=", ")

      
        
      RoachRoleCall <- RoachRoleCall + 1
    }
    output$List_of_Roaches<- renderText({
      paste("<span style='font-size: 10px;'>Roaches ", RoachCoachList,"</span>")
    })        
    
    ### All Roaches Checked-in - :)
    
#    showDialog(title ="The 3-D Roach Motel", message = 
#                 paste("<p>All roaches are in their Assigned Room and the last roach to <b>Check-in</b> to its room was Roach #", LastRoach ," at step ", maxsteps ,
#                       ".</p><p>When you click OK the Motel will start building.  If you picked a large amount of Roaches, this might take some time. Be Patient!!</p><p>You can read my paper</p> <p><b>The Roach Motel with Chutes and Ladders and with Early and Late Check-in</b></p><p>
# and its <b>proof of a popular previously unproven conjecture</b> at:</p>", sep="")
#               ,url = "https://"
#    )
    
    themotel
    })
        

      output$plot3D <-renderPlotly({
#      themotel})
    Plot_it()})
    
}

# Run the application
shinyApp(ui = ui, server = server)


