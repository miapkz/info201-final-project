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

top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20)

chart <- top_20_companies %>%
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>%
  group_by(parent_company) %>%
  summarize(total = sum(company_total_plastics, na.rm = TRUE))

# Group all countries and sum the # of events and volunteers
events_and_volunteers_per_country <- plastics %>%
  group_by(country) %>%
  summarize(num_events = sum(num_events, na.rm = TRUE), num_volunteers = sum(volunteers, na.rm = TRUE)) %>%
  filter(!country %in% c("EMPTY"))

# # Create a scatter plot of events and volunteers to show specific countries as outliers
# ggplot(events_and_volunteers_per_country, aes(x = num_events, y = num_volunteers)) +
#   geom_point(aes(color = country)) +
#   geom_text(aes(label = country), hjust = -0.2, vjust = 0.5) +
#   labs(title = "Number of Events vs Number of Volunteers by Country", x = "Number of Events", y = "Number of Volunteers") +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
#   scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

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
  count = c(data_2019$hdpe, data_2019$ldpe, data_2019$pet, 
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

server <- function(input, output) {
  output$plot <- renderPlotly({
    # top_20_companies <- plastics %>%
    # group_by(parent_company) %>%
    # summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
    # arrange(desc(company_total_plastics)) %>%
    # filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>%
    # filter(row_number() <= 20)

    chart <- top_20_companies %>%
      filter(parent_company %in% input$user_selection) %>%
      group_by(parent_company) %>%
      summarize(total = sum(company_total_plastics))

    plot <- ggplot(data = chart) +
      geom_col(mapping = aes(x = reorder(parent_company, +total), y = total, fill = parent_company)) +
      coord_flip() +
      labs(title = "Top 20 Plastic-Emitting Companies", x = "Parent Company", y = "Total Plastic Emitted (million metric tons)") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = label_number_si())

    return(ggplotly(plot, tooltip = c("y", "million metric tons")))
  })

  plotlyOutput(outputId = "plot")

  output$plot2 <- renderPlotly({
    chart2 <- events_and_volunteers_per_country %>%
      filter(country %in% input$panel2_selection)


    plot2 <- ggplot(data = chart2, aes(x = num_events, y = num_volunteers)) +
      geom_point(aes(color = country)) +
      labs(
        title = "Number of Events vs Number of Volunteers by Country",
        x = "Number of Events",
        y = "Number of Volunteers"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
      scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))


    return(ggplotly(plot2))
  })

  plotOutput(outputId = "plot2")

  output$plot_3 <- renderPlotly({
    if (input$year_selection_3 == "2019") {
      filtered_3 <- plot_3_df_1 %>% filter(type %in% input$type_selection)
    } else {
      filtered_3 <- plot_3_df_2 %>% filter(type %in% input$type_selection)
    }
    p3 <- ggplot(data = filtered_3, aes(x = type, y = count, fill = type)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = paste(
          "Total Count of Different Types of Plastics in",
          input$year_selection_3
        ),
        x = "Type of Plastics",
        y = "Total Count of Each Type of Plastic"
      ) +
      scale_y_continuous(labels = label_number_si())
    return(ggplotly(p3, tooltip = c("x", "y")))
  })
  plotlyOutput(outputId = "plot_3")

  # Conclusion
  output$Takeaways <- renderUI({
    HTML("<h2>Key Findings</h2>
          <h4>We initially sought to answer the following questions:</h4>
          <ul>
            <li>Which company was responsible for the most amount of plastic?</li>
            <li>Which country had the most amount of events and volunteers for plastic collection?</li>
            <li>What type of plastic was collected the most overall?</li>
          </ul>
          <p>Our critical research questions aimed to identify the brands responsible for the most plastic pollution, allowing for targeted efforts to reduce their impact. With increased awareness of the issue of single-use plastics, we extracted information from a dataset to explore the types of plastic and locations most affected, revealing crucial insights for addressing the issue of plastic pollution. By identifying the companies at fault, our research provides a basis for holding them accountable and seeking ways to lessen their effect on the environment. Our findings have important implications for raising awareness and driving change.</p>
          <h2>Specific Takeaways</h2>
          <ul>
            <li>The Coca-Cola Parent Company was responsible for the most plastic emissions (<b>25,530 million metric tons</b>)</li>
            <li>The United States of America had the highest number of events (<b>74,870</b>), yet a fairly low number of volunteers in relation (<b>623,556</b>)</li>
            <li>Indonesia had the highest total number of volunteers (<b>4,471,848</b>) with only <b>35,052</b> events.</li>
            <li>The highest plastic-type emitted was Polyester (PET) in 2019 (<b>193,796</b>).</li>
          </ul>
          <p>Insights from our research into the amount and types of plastic produced by companies have broader implications for technologists, designers, and policymakers. Technologists can use the data to develop new technologies and products that reduce plastic waste, while designers can use it to design more sustainable products. Policymakers can use the data to inform regulations and policies aimed at reducing plastic waste and to track progress over time with a focus on the mass-producing companies we discovered.</p>
          <p>In conclusion, while this data provides valuable insights in the amount and types of plastic pollution and their main sources, it is important to note that it is not entirely comprehensive and may not accurately reflect the full extent of plastic pollution each year. However, even with the data present here, the amount of plastic emissions is unacceptable and will ultimately lead to our planet's demise. Change is crucial.</p>")
  })
}