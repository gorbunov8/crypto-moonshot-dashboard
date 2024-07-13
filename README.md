# Global Market Insights Dashboard

**A Shiny dashboard to provide comprehensive insights into global stock markets, including market indices, changes, and volatility.**

---

## Project Overview

The **Global Market Insights Dashboard** aims to offer an in-depth analysis of global stock markets. The dashboard allows users to:

1. **View market indices**: Explore the latest market index values for various countries on a map.
2. **Analyze market changes**: Examine changes in market indices over 3-month and 12-month periods.
3. **Assess stock price volatility**: Understand the volatility of stock prices across different countries.
4. **Forecast S&P 500 trends**: Utilize historical data to forecast future trends in the S&P 500 index and calculate potential investment returns.

---

## Note

This dashboard is for informational purposes only and should not be considered financial advice.

---

## Getting Started

1. **Install Required Packages**:
   ```r
   install.packages(c("shiny", "leaflet", "shinydashboard", "shinyWidgets", "rvest", "readr", "leaflet.extras", "DT", "dplyr", "ggplot2", "plotly", "forecast"))
   ```
   
2. **Run the Application**:
   ```r
   shinyApp(ui, server)
   ```   
