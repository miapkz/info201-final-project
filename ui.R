library(shiny)
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("bslib")
library("markdown")
# Get the original data from Tidy Tuesday
tuesdata <- tidytuesdayR::tt_load("2021-01-26")

# Extract the plastics data
plastics <- tuesdata$plastics

# Filter data for plot 1
top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20)

by_type <- plastics %>%
  group_by(year) %>%
  summarize(
    hdpe = sum(hdpe, na.rm = TRUE),
    ldpe = sum(ldpe, na.rm = TRUE),
    pet = sum(pet, na.rm = TRUE),
    pp = sum(pp, na.rm = TRUE),
    ps = sum(ps, na.rm = TRUE),
    pvc = sum(pvc, na.rm = TRUE)
  )
data_2019 <- by_type %>%
  filter(year == 2019)
data_2020 <- by_type %>%
  filter(year == 2020)
plot_3_df_1 <- data.frame(
  type = c("hdpe", "ldpe", "pet", "pp", "ps", "pvc"),
  count = c(
    data_2019$hdpe, data_2019$ldpe, data_2019$pet,
    data_2019$pp, data_2019$ps, data_2019$pvc
  )
)
plot_3_df_2 <- data.frame(
  type = c("hdpe", "ldpe", "pet", "pp", "ps", "pvc"),
  count = c(
    data_2020$hdpe, data_2020$ldpe, data_2020$pet,
    data_2020$pp, data_2020$ps, data_2020$pvc
  )
)
# Group all countries and sum the # of events and volunteers
events_and_volunteers_per_country <- plastics %>%
  group_by(country) %>%
  summarize(num_events = sum(num_events, na.rm = TRUE), num_volunteers = sum(volunteers, na.rm = TRUE)) %>%
  filter(!country %in% c("EMPTY"))

# Create a scatter plot of events and volunteers to show specific countries as outliers
ggplot(events_and_volunteers_per_country, aes(x = num_events, y = num_volunteers)) +
  geom_point(aes(color = country)) +
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.5) +
  labs(title = "Number of Events vs Number of Volunteers by Country", x = "Number of Events", y = "Number of Volunteers") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
  scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

my_theme <- bs_theme(bg = "#0b3d91",
                     fg = "white", 
                     primary = "#FCC780")
my_theme <- bs_theme_update(my_theme, bootswatch = "flatly")

# Application title
intro_panel <- tabPanel(
  "Introduction",
  fluidPage(
    includeMarkdown("intro.Rmd"),
    tags$image(src = "plasticphoto.jpg", height = 500, width = 500, ))
  )

# An introductory page that provides an overview of the project. What major questions are you seeking to answer? What data are you using to answer those questions? Please provide a URL link to the original source(s) of the data. Where did the data come from, and what are possible ethical questions or limitations to consider with this dataset? You should also include some additional "flare" on this landing page, such as an image.

plot_panel <- tabPanel(
  "Emissions by Company",
  sidebarLayout(
     sidebarPanel(
        selectInput(
        inputId = "user_selection",
        label = "Select Company",
        choices = top_20_companies$parent_company,
        selected = "The Coca-Cola Company",
        multiple = TRUE
        )
      ),
    mainPanel(
      plotlyOutput("plot"),
      p("This visualization represents the top 20 plastic-emitting companies and how much total million metric tons of plastic waste was recorded for them in 2019 and 2020. As we can see, The Coca-Cola Company is by far the largest emittor of plastic, with 25530 millions metric tons emitted in these two years. Other prominent plastic pollutors include Universal Robina Corporation, Nestle, PepsiCo and Colgate-Palmolive.")
    )
  )
 
)

plot_panel_2 <- tabPanel(
  "Number of Events vs Number of Volunteers",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "panel2_selection",
        label = "Select Country",
        choices = events_and_volunteers_per_country$country,
        selected = as.character(c(
          "United States of America", "China",
          "Argentina", "United Kingdom", "Kenya"
        )),
        multiple = TRUE
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "plot2"),
      p("This scatter plot depicts each country’s number of events and number of volunteers. By looking at the graph you can recognize which countries seem to be holding more events and how well those events are attended. The two outliers on the plot are China and the US, though they are far from each other. China’s data illustrates a lower number of events, but had many more volunteers, while the US was the opposite.")
    )
  )
  
)



plot_panel_3 <- tabPanel(
  "Emissions by Plastic Type",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "year_selection_3",
        label = "Select Year",
        choices = c("2019", "2020"),
        selected = "2019",
        multiple = FALSE
      ),
      selectInput(
        inputId = "type_selection",
        label = "Select Plastic Type",
        choices = plot_3_df_1$type,
        selected = "pet",
        multiple = TRUE
      ),
      p("To avoid having a messy plot, we used abbreviations names for the different types of plastics. Here's the meaning of these abbreviations and what each type of plastics is commonly used for:"),
      p("hdpe: high density polyethylene. This is used in plastic milk containers, plastic bags, bottle caps, trash cans, etc."),
      p("ldpe: low density polyethylene. This is used in plastic bags, Ziploc bags, buckets, squeeze bottles, etc."),
      p("pet: Polyester plastic count. This is used in Polyester fibers, soft drink bottles, food containers, etc. (This is the default choice)"),
      p("pp: Polypropylene. This is used in flower pots, bumpers, carry-out beverage cupss, microwavable food containers, etc."),
      p("ps: Polystyrene. This is used in toys, video cassettes, beverage/food coolers, beer cups, carry-out food containers, etc."),
      p("pvc: PVC plastic. This is used in window frames, bottles for chemicals, flooring, plumbing pipes, etc.")
    ),
    mainPanel(
      plotlyOutput("plot_3"),
      p("This chart allows us to look at the count of different types of plastics by type. Since we only have the data of 2019 and 2020, it is hard to see the trend of count by time. Therefore, we made a graph for counts in 2019 and another graph for 2020. The default graph is for 2019. Users could change between the two graphs by changing the option of the first widget."),
      p("The second widget controls which particular types of plastics users would like to see on the graph. The default choice is showing only polyester plastic. Users could choose up to having all categories show up on the graph. Clicking on the color bar of the legend on the right side could also hide a bar from showing on the graph. However, that won't take the category off the graph. "),
      p("From these graphs we could have an idea of what plastic has a higher count overall and start forming plans to cut down the usage or reuse these plastics. Some of the plastics are recycable and some are not. By looking at the bars, we would be able to have a clearer idea of what the next steps should be. ")
    )
  )
)
  

conclusion_panel <- tabPanel(
  "Takeaways",
  htmlOutput("Takeaways")
)
# A conclusion page of summary takeaways that hones in on at least 3 major takeaways from the project, which should be related to a specific aspect of your analysis. This page should include at least 250 words of text, but feel free to incorporate tables, graphics, or other elements necessary to convey your conclusions.

ui <- navbarPage(
  theme = my_theme,
  "Plastic Pollution Around the World",
  intro_panel,
  plot_panel,
  plot_panel_2,
  plot_panel_3,
  conclusion_panel
)
