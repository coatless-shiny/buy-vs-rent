library(shiny)
library(plotly)
library(DT)
library(dplyr)

# Define server logic
server <- function(input, output, session) {

    # Reactive calculations
    calculations <- reactive({
        # Basic mortgage calculations
        loan_amount <- input$home_price * (1 - input$down_payment_pct/100)
        monthly_rate <- input$interest_rate / 100 / 12
        num_payments <- input$loan_term * 12

        # Monthly mortgage payment (principal and interest)
        monthly_pi <- loan_amount * (monthly_rate * (1 + monthly_rate)^num_payments) /
            ((1 + monthly_rate)^num_payments - 1)

        # Additional monthly costs
        monthly_property_tax <- input$home_price * input$property_tax_rate / 100 / 12
        monthly_insurance <- input$insurance_annual / 12
        monthly_maintenance <- input$home_price * input$maintenance_pct / 100 / 12

        # Calculate down payment upfront
        down_payment <- input$home_price * input$down_payment_pct / 100

        # Calculate costs over time with PMI and HOA
        years <- 1:input$time_horizon

        # PMI calculations - only if down payment < 20%
        pmi_required <- input$down_payment_pct < 20
        initial_pmi_monthly <- if(pmi_required) {
            loan_amount * input$pmi_rate / 100 / 12
        } else {
            0
        }

        # Rent costs (with annual increases)
        rent_costs <- sapply(years, function(y) {
            annual_rent <- input$monthly_rent * 12 * (1 + input$rent_increase/100)^(y-1)
            annual_rent
        })

        # Calculate monthly costs year by year (including PMI removal and HOA increases)
        buy_costs <- do.call(rbind, lapply(years, function(y) {
            # Home value at end of year
            home_value <- input$home_price * (1 + input$home_appreciation/100)^y

            # Remaining loan balance at end of year
            remaining_balance <- loan_amount * ((1 + monthly_rate)^num_payments - (1 + monthly_rate)^(y*12)) /
                ((1 + monthly_rate)^num_payments - 1)

            # Calculate monthly costs for this year
            monthly_costs_this_year <- numeric(12)
            total_pmi_this_year <- 0
            total_hoa_this_year <- 0

            for(month in 1:12) {
                month_number <- (y-1)*12 + month

                # Calculate remaining balance at this specific month
                remaining_balance_month <- loan_amount * ((1 + monthly_rate)^num_payments - (1 + monthly_rate)^month_number) /
                    ((1 + monthly_rate)^num_payments - 1)

                # Calculate home value at this month (linear interpolation within year)
                home_value_month <- input$home_price * (1 + input$home_appreciation/100)^((month_number-1)/12)

                # Calculate LTV ratio
                ltv_ratio <- remaining_balance_month / home_value_month

                # PMI for this month (with annual increases, removed at 78% LTV)
                pmi_this_month <- if(pmi_required && ltv_ratio > 0.78) {
                    annual_pmi_rate <- input$pmi_rate * (1 + input$pmi_increase/100)^(y-1)
                    loan_amount * annual_pmi_rate / 100 / 12
                } else {
                    0
                }

                # HOA for this month (with annual increases)
                hoa_this_month <- input$hoa_monthly * (1 + input$hoa_increase/100)^(y-1)

                total_pmi_this_year <- total_pmi_this_year + pmi_this_month
                total_hoa_this_year <- total_hoa_this_year + hoa_this_month

                monthly_costs_this_year[month] <- monthly_pi + monthly_property_tax +
                    monthly_insurance + monthly_maintenance + pmi_this_month + hoa_this_month
            }

            # Annual ownership costs
            annual_own <- sum(monthly_costs_this_year)

            # Equity built
            equity <- home_value - remaining_balance

            # Net cost (costs minus equity, plus opportunity cost of down payment)
            opportunity_cost <- down_payment * ((1 + input$investment_return/100)^y - 1)

            if(y == 1) {
                total_costs <- down_payment + input$closing_costs + annual_own
            } else {
                total_costs <- annual_own
            }

            data.frame(
                year = y,
                annual_cost = annual_own,
                cumulative_cost = total_costs,
                home_value = home_value,
                equity = equity,
                opportunity_cost = opportunity_cost,
                annual_pmi = total_pmi_this_year,
                annual_hoa = total_hoa_this_year,
                average_monthly_payment = annual_own / 12
            )
        }))

        # Calculate current year 1 monthly costs for display
        current_pmi_monthly <- if(pmi_required) {
            loan_amount * input$pmi_rate / 100 / 12
        } else {
            0
        }
        current_hoa_monthly <- input$hoa_monthly

        total_monthly_own <- monthly_pi + monthly_property_tax + monthly_insurance +
            monthly_maintenance + current_pmi_monthly + current_hoa_monthly

        list(
            loan_amount = loan_amount,
            monthly_pi = monthly_pi,
            monthly_property_tax = monthly_property_tax,
            monthly_insurance = monthly_insurance,
            monthly_maintenance = monthly_maintenance,
            monthly_pmi = current_pmi_monthly,
            monthly_hoa = current_hoa_monthly,
            total_monthly_own = total_monthly_own,
            rent_costs = rent_costs,
            buy_costs = buy_costs,
            down_payment = down_payment,
            pmi_required = pmi_required
        )
    })

    # Recommendation output
    output$recommendation <- renderText({
        calc <- calculations()

        # Calculate total cost of renting vs buying over time horizon
        total_rent_cost <- sum(calc$rent_costs)

        # For buying, calculate net cost including opportunity cost
        final_equity <- calc$buy_costs$equity[input$time_horizon]
        final_opportunity_cost <- calc$buy_costs$opportunity_cost[input$time_horizon]
        total_buy_cost <- calc$down_payment + input$closing_costs +
            sum(calc$buy_costs$annual_cost)

        # Subtract final equity and add opportunity cost
        net_buy_cost <- total_buy_cost - final_equity + final_opportunity_cost

        if(net_buy_cost < total_rent_cost) {
            "BUY"
        } else {
            "RENT"
        }
    })

    # Net benefit output
    output$net_benefit <- renderText({
        calc <- calculations()

        total_rent_cost <- sum(calc$rent_costs)
        final_equity <- calc$buy_costs$equity[input$time_horizon]
        final_opportunity_cost <- calc$buy_costs$opportunity_cost[input$time_horizon]
        total_buy_cost <- calc$down_payment + input$closing_costs +
            sum(calc$buy_costs$annual_cost)
        net_buy_cost <- total_buy_cost - final_equity + final_opportunity_cost

        benefit <- total_rent_cost - net_buy_cost

        paste0("$", format(round(benefit), big.mark = ","))
    })

    # PMI information
    output$pmi_info <- renderText({
        calc <- calculations()
        if(calc$pmi_required) {
            paste0("PMI required (down payment < 20%). ",
                   "PMI will be automatically removed when loan-to-value ratio reaches 78%.")
        } else {
            "No PMI required (down payment ≥ 20%)."
        }
    })

    # Cost comparison plot
    output$cost_plot <- renderPlotly({
        calc <- calculations()

        years <- 1:input$time_horizon
        cumulative_rent <- cumsum(calc$rent_costs)

        cumulative_buy <- sapply(years, function(y) {
            costs_up_to_y <- calc$down_payment + input$closing_costs +
                sum(calc$buy_costs$annual_cost[1:y])
            equity_at_y <- calc$buy_costs$equity[y]
            opportunity_cost_at_y <- calc$buy_costs$opportunity_cost[y]

            costs_up_to_y - equity_at_y + opportunity_cost_at_y
        })

        df <- data.frame(
            Year = years,
            Rent = cumulative_rent,
            Buy = cumulative_buy
        )

        p <- plot_ly(df, x = ~Year) %>%
            add_lines(y = ~Rent, name = "Rent", line = list(color = "#E74C3C")) %>%
            add_lines(y = ~Buy, name = "Buy", line = list(color = "#2C3E50")) %>%
            layout(
                title = "Cumulative Net Cost Over Time",
                xaxis = list(title = "Years"),
                yaxis = list(title = "Cumulative Net Cost ($)"),
                hovermode = 'x unified'
            )

        p
    })

    # Cash flow plot
    output$cashflow_plot <- renderPlotly({
        calc <- calculations()

        years <- 1:input$time_horizon

        df <- data.frame(
            Year = years,
            Monthly_Rent = rep(input$monthly_rent, length(years)) * (1 + input$rent_increase/100)^(years-1),
            Monthly_Own = calc$buy_costs$average_monthly_payment
        )

        p <- plot_ly(df, x = ~Year) %>%
            add_bars(y = ~Monthly_Rent, name = "Monthly Rent", marker = list(color = "#E74C3C")) %>%
            add_bars(y = ~Monthly_Own, name = "Monthly Ownership", marker = list(color = "#2C3E50")) %>%
            layout(
                title = "Monthly Housing Costs (including PMI removal)",
                xaxis = list(title = "Year"),
                yaxis = list(title = "Monthly Cost ($)"),
                barmode = 'group'
            )

        p
    })

    # Breakdown table
    output$breakdown_table <- DT::renderDataTable({
        calc <- calculations()

        years <- 1:input$time_horizon

        df <- data.frame(
            Year = years,
            Annual_Rent = calc$rent_costs,
            Annual_Ownership = calc$buy_costs$annual_cost,
            Annual_PMI = calc$buy_costs$annual_pmi,
            Annual_HOA = calc$buy_costs$annual_hoa,
            Home_Value = calc$buy_costs$home_value,
            Equity_Built = calc$buy_costs$equity,
            Opportunity_Cost = calc$buy_costs$opportunity_cost
        )

        # Format as currency
        df[, 2:8] <- lapply(df[, 2:8], function(x) paste0("$", format(round(x), big.mark = ",")))

        DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                      colnames = c("Year", "Annual Rent", "Annual Ownership", "Annual PMI",
                                   "Annual HOA", "Home Value", "Equity Built", "Opportunity Cost"))
    })

    # Mortgage calculations for guide tab
    output$loan_amount <- renderText({
        calc <- calculations()
        paste0("$", format(round(calc$loan_amount), big.mark = ",", scientific = FALSE))
    })

    output$monthly_payment <- renderText({
        calc <- calculations()
        paste0("$", format(round(calc$monthly_pi), big.mark = ","))
    })

    output$total_interest <- renderText({
        calc <- calculations()
        total_payments <- calc$monthly_pi * input$loan_term * 12
        total_interest <- total_payments - calc$loan_amount
        paste0("$", format(round(total_interest), big.mark = ","))
    })

    output$payment_breakdown <- renderTable({
        calc <- calculations()

        components <- c("Principal & Interest", "Property Tax", "Insurance", "Maintenance")
        amounts <- c(
            paste0("$", format(round(calc$monthly_pi), big.mark = ",")),
            paste0("$", format(round(calc$monthly_property_tax), big.mark = ",")),
            paste0("$", format(round(calc$monthly_insurance), big.mark = ",")),
            paste0("$", format(round(calc$monthly_maintenance), big.mark = ","))
        )

        # Add PMI if required
        if(calc$pmi_required) {
            components <- c(components, "PMI")
            amounts <- c(amounts, paste0("$", format(round(calc$monthly_pmi), big.mark = ",")))
        }

        # Add HOA if > 0
        if(input$hoa_monthly > 0) {
            components <- c(components, "HOA")
            amounts <- c(amounts, paste0("$", format(round(calc$monthly_hoa), big.mark = ",")))
        }

        # Add total
        components <- c(components, "Total Monthly")
        amounts <- c(amounts, paste0("$", format(round(calc$total_monthly_own), big.mark = ",")))

        data.frame(
            Component = components,
            Amount = amounts
        )
    }, colnames = FALSE)

    # Summary payment breakdown for main tab
    output$payment_breakdown_summary <- renderTable({
        calc <- calculations()

        components <- c("Principal & Interest", "Property Tax", "Insurance", "Maintenance")
        amounts <- c(
            paste0("$", format(round(calc$monthly_pi), big.mark = ",")),
            paste0("$", format(round(calc$monthly_property_tax), big.mark = ",")),
            paste0("$", format(round(calc$monthly_insurance), big.mark = ",")),
            paste0("$", format(round(calc$monthly_maintenance), big.mark = ","))
        )

        # Add PMI if required
        if(calc$pmi_required) {
            components <- c(components, "PMI")
            amounts <- c(amounts, paste0("$", format(round(calc$monthly_pmi), big.mark = ",")))
        }

        # Add HOA if > 0
        if(input$hoa_monthly > 0) {
            components <- c(components, "HOA")
            amounts <- c(amounts, paste0("$", format(round(calc$monthly_hoa), big.mark = ",")))
        }

        # Add total
        components <- c(components, "Total Monthly")
        amounts <- c(amounts, paste0("$", format(round(calc$total_monthly_own), big.mark = ",")))

        data.frame(
            Component = components,
            Amount = amounts
        )
    }, colnames = FALSE, striped = TRUE)

    # Amortization calculations
    amortization_data <- reactive({
        calc <- calculations()
        loan_amount <- calc$loan_amount
        monthly_rate <- input$interest_rate / 100 / 12
        num_payments <- input$loan_term * 12
        monthly_payment <- calc$monthly_pi

        # Monthly amortization schedule
        monthly_schedule <- data.frame(
            Payment_Number = 1:num_payments,
            Month = 1:num_payments,
            Year = ceiling((1:num_payments) / 12)
        )

        monthly_schedule$Beginning_Balance <- numeric(num_payments)
        monthly_schedule$Interest_Payment <- numeric(num_payments)
        monthly_schedule$Principal_Payment <- numeric(num_payments)
        monthly_schedule$Ending_Balance <- numeric(num_payments)
        monthly_schedule$Cumulative_Interest <- numeric(num_payments)
        monthly_schedule$Cumulative_Principal <- numeric(num_payments)

        balance <- loan_amount
        cumulative_interest <- 0
        cumulative_principal <- 0

        for(i in 1:num_payments) {
            monthly_schedule$Beginning_Balance[i] <- balance
            interest_payment <- balance * monthly_rate
            principal_payment <- monthly_payment - interest_payment
            balance <- balance - principal_payment

            cumulative_interest <- cumulative_interest + interest_payment
            cumulative_principal <- cumulative_principal + principal_payment

            monthly_schedule$Interest_Payment[i] <- interest_payment
            monthly_schedule$Principal_Payment[i] <- principal_payment
            monthly_schedule$Ending_Balance[i] <- max(0, balance)
            monthly_schedule$Cumulative_Interest[i] <- cumulative_interest
            monthly_schedule$Cumulative_Principal[i] <- cumulative_principal
        }

        # Yearly amortization schedule
        yearly_schedule <- monthly_schedule %>%
            group_by(Year) %>%
            summarise(
                Beginning_Balance = first(Beginning_Balance),
                Total_Payment = n() * monthly_payment,
                Interest_Payment = sum(Interest_Payment),
                Principal_Payment = sum(Principal_Payment),
                Ending_Balance = last(Ending_Balance),
                Cumulative_Interest = last(Cumulative_Interest),
                Cumulative_Principal = last(Cumulative_Principal),
                .groups = 'drop'
            )

        list(monthly = monthly_schedule, yearly = yearly_schedule)
    })

    # Yearly amortization table
    output$yearly_amortization_table <- DT::renderDataTable({
        amort <- amortization_data()
        df <- amort$yearly

        # Format currency columns
        currency_cols <- c("Beginning_Balance", "Total_Payment", "Interest_Payment",
                           "Principal_Payment", "Ending_Balance", "Cumulative_Interest",
                           "Cumulative_Principal")

        df[currency_cols] <- lapply(df[currency_cols], function(x) {
            paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
        })

        DT::datatable(df,
                      rownames = FALSE,
                      options = list(pageLength = 15, scrollX = TRUE),
                      colnames = c("Year", "Beginning Balance", "Total Payment",
                                   "Interest Payment", "Principal Payment", "Ending Balance",
                                   "Cumulative Interest", "Cumulative Principal"))
    })

    # Monthly amortization table
    output$monthly_amortization_table <- DT::renderDataTable({
        amort <- amortization_data()
        df <- amort$monthly

        # Format currency columns
        currency_cols <- c("Beginning_Balance", "Interest_Payment", "Principal_Payment",
                           "Ending_Balance", "Cumulative_Interest", "Cumulative_Principal")

        df[currency_cols] <- lapply(df[currency_cols], function(x) {
            paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
        })

        DT::datatable(df,
                      rownames = FALSE,
                      options = list(pageLength = 12, scrollX = TRUE),
                      colnames = c("Payment #", "Month", "Year", "Beginning Balance",
                                   "Interest Payment", "Principal Payment", "Ending Balance",
                                   "Cumulative Interest", "Cumulative Principal"))
    })

    # Yearly amortization plot
    output$yearly_amortization_plot <- renderPlotly({
        amort <- amortization_data()
        df <- amort$yearly

        p <- plot_ly(df, x = ~Year) %>%
            add_bars(y = ~Interest_Payment, name = "Interest Payment",
                     marker = list(color = "#E74C3C")) %>%
            add_bars(y = ~Principal_Payment, name = "Principal Payment",
                     marker = list(color = "#2C3E50")) %>%
            layout(
                title = "Yearly Principal vs Interest Payments",
                xaxis = list(title = "Year"),
                yaxis = list(title = "Annual Payment ($)"),
                barmode = 'stack',
                hovermode = 'x unified'
            )

        p
    })

    # Monthly amortization plot
    output$monthly_amortization_plot <- renderPlotly({
        amort <- amortization_data()
        df <- amort$monthly

        # Limit to first 10 years for readability if loan is longer
        display_months <- min(120, nrow(df))
        df_display <- df[1:display_months, ]

        p <- plot_ly(df_display, x = ~Payment_Number) %>%
            add_lines(y = ~Interest_Payment, name = "Interest Payment",
                      line = list(color = "#E74C3C")) %>%
            add_lines(y = ~Principal_Payment, name = "Principal Payment",
                      line = list(color = "#2C3E50")) %>%
            add_lines(y = ~Ending_Balance, name = "Remaining Balance",
                      line = list(color = "#18BC9C"), yaxis = "y2") %>%
            layout(
                title = paste0("Monthly Payment Breakdown",
                               if(display_months < nrow(df)) " (First 10 Years)" else ""),
                xaxis = list(title = "Payment Number"),
                yaxis = list(title = "Payment Amount ($)", side = "left"),
                yaxis2 = list(title = "Remaining Balance ($)", side = "right", overlaying = "y"),
                hovermode = 'x unified'
            )

        p
    })
}
