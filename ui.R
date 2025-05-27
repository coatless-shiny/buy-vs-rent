library(shiny)
library(bslib)
library(plotly)
library(DT)

# Define UI
ui <- page_sidebar(
    title = "Buy vs Rent Decision Tool",

    # Sidebar with collapsible sections
    sidebar = sidebar(
        width = 350,

        # Main recommendation at top of sidebar
        value_box(
            title = "Recommendation",
            value = textOutput("recommendation"),
            showcase = icon("home"),
            theme = "primary",
            height = "100px"
        ),

        br(),

        # Collapsible input sections
        accordion(
            accordion_panel(
                title = "Property & Location",
                icon = icon("map-marker-alt"),
                numericInput("home_price", "Home Price ($)", value = 500000, min = 50000, step = 10000),
                numericInput("monthly_rent", "Monthly Rent ($)", value = 2500, min = 500, step = 100)
            ),

            accordion_panel(
                title = "Financing",
                icon = icon("credit-card"),
                numericInput("down_payment_pct", "Down Payment (%)", value = 20, min = 0, max = 100, step = 5),
                numericInput("interest_rate", "Mortgage Interest Rate (%)", value = 6.5, min = 1, max = 15, step = 0.1),
                numericInput("loan_term", "Loan Term (years)", value = 30, min = 10, max = 50, step = 5)
            ),

            accordion_panel(
                title = "Additional Costs",
                icon = icon("calculator"),
                numericInput("property_tax_rate", "Property Tax Rate (%/year)", value = 1.2, min = 0, max = 5, step = 0.1),
                numericInput("insurance_annual", "Home Insurance ($/year)", value = 1200, min = 0, step = 100),
                numericInput("maintenance_pct", "Maintenance Cost (%/year of home value)", value = 1, min = 0, max = 5, step = 0.1),
                numericInput("closing_costs", "Closing Costs ($)", value = 10000, min = 0, step = 1000),

                # PMI inputs
                hr(),
                h5("Private Mortgage Insurance (PMI)"),
                p(class = "text-muted small", "PMI is required when down payment < 20% and is removed when loan-to-value reaches 78%"),
                numericInput("pmi_rate", "PMI Rate (%/year of loan amount)", value = 0.5, min = 0, max = 2, step = 0.1),
                numericInput("pmi_increase", "Annual PMI Increase (%)", value = 0, min = 0, max = 10, step = 0.5),

                # HOA inputs
                hr(),
                h5("Homeowners Association (HOA)"),
                numericInput("hoa_monthly", "Monthly HOA Fee ($)", value = 0, min = 0, step = 25),
                numericInput("hoa_increase", "Annual HOA Increase (%)", value = 3, min = 0, max = 15, step = 0.5)
            ),

            accordion_panel(
                title = "Market Assumptions",
                icon = icon("chart-line"),
                numericInput("home_appreciation", "Home Appreciation Rate (%/year)", value = 3, min = -5, max = 10, step = 0.5),
                numericInput("rent_increase", "Annual Rent Increase (%)", value = 3, min = 0, max = 10, step = 0.5),
                numericInput("investment_return", "Alternative Investment Return (%/year)", value = 7, min = 0, max = 15, step = 0.5),
                numericInput("time_horizon", "Time Horizon (years)", value = 10, min = 1, max = 30, step = 1)
            )
        )
    ),

    # Main content area with tabs
    navset_card_tab(
        nav_panel(
            title = "Financial Analysis",
            icon = icon("chart-bar"),
            layout_columns(
                # Summary metrics
                col_widths = c(6, 6),
                value_box(
                    title = "Net Benefit of Buying",
                    value = textOutput("net_benefit"),
                    showcase = icon("dollar-sign"),
                    theme = "secondary"
                ),
                value_box(
                    title = "Monthly Payment (P&I)",
                    value = textOutput("monthly_payment"),
                    showcase = icon("calendar-check"),
                    theme = "primary"
                )
            ),

            br(),

            # Charts
            layout_columns(
                col_widths = c(6, 6),
                card(
                    card_header("Cost Comparison Over Time"),
                    card_body(
                        plotlyOutput("cost_plot", height = "400px")
                    )
                ),
                card(
                    card_header("Monthly Housing Costs"),
                    card_body(
                        plotlyOutput("cashflow_plot", height = "400px")
                    )
                )
            ),

            br(),

            # Monthly Payment Breakdown Treemap
            card(
                card_header("Monthly Payment Breakdown"),
                card_body(
                    plotlyOutput("payment_breakdown_treemap", height = "400px")
                )
            )
        ),

        nav_panel(
            title = "Mortgage Details",
            icon = icon("home"),
            layout_columns(
                col_widths = c(3, 3, 3, 3),

                value_box(
                    title = "Loan Amount",
                    value = textOutput("loan_amount"),
                    showcase = icon("calculator"),
                    theme = "primary"
                ),

                value_box(
                    title = "Total Interest Paid",
                    value = textOutput("total_interest"),
                    showcase = icon("percentage"),
                    theme = "info"
                ),

                value_box(
                    title = "Total Cost of Loan",
                    value = textOutput("total_loan_cost"),
                    showcase = icon("money-bill-wave"),
                    theme = "warning"
                ),

                value_box(
                    title = "Payoff Date",
                    value = textOutput("payoff_date"),
                    showcase = icon("calendar-alt"),
                    theme = "success"
                )
            ),

            br(),

            layout_columns(
                col_widths = c(6, 6),

                card(
                    card_header("Mortgage Process Overview"),
                    card_body(
                        h4("Step 1: Pre-Approval"),
                        p("Get pre-approved for a mortgage to understand your budget and show sellers you're serious."),
                        tags$ul(
                            tags$li("Check your credit score (aim for 620+ for conventional loans)"),
                            tags$li("Gather financial documents (W-2s, pay stubs, bank statements)"),
                            tags$li("Calculate debt-to-income ratio (should be <43%)"),
                            tags$li("Shop around with multiple lenders for best rates")
                        ),

                        h4("Step 2: House Hunting"),
                        p("Use your pre-approval amount to guide your search, but consider total monthly costs."),

                        h4("Step 3: Make an Offer"),
                        p("Include financing contingency and home inspection contingency in your offer."),

                        h4("Step 4: Loan Processing"),
                        tags$ul(
                            tags$li("Submit full application with all required documents"),
                            tags$li("Property appraisal will be ordered by lender"),
                            tags$li("Loan underwriting process (can take 30-45 days)"),
                            tags$li("Final loan approval and clear to close")
                        ),

                        h4("Step 5: Closing"),
                        p("Final walkthrough, sign documents, transfer funds, and receive keys!")
                    )
                ),

                card(
                    card_header("Detailed Payment Breakdown"),
                    card_body(
                        h4("Monthly Costs:"),
                        tableOutput("payment_breakdown"),
                        br(),
                        h5("PMI Information:"),
                        textOutput("pmi_info")
                    )
                )
            )
        ),

        nav_panel(
            title = "Amortization",
            icon = icon("table"),
            navset_card_tab(
                nav_panel(
                    title = "By Year",
                    layout_columns(
                        col_widths = c(12),
                        card(
                            card_header("Yearly Amortization Chart"),
                            card_body(
                                plotlyOutput("yearly_amortization_plot", height = "400px")
                            )
                        )
                    ),
                    br(),
                    card(
                        card_header("Yearly Amortization Schedule"),
                        card_body(
                            DT::dataTableOutput("yearly_amortization_table")
                        )
                    )
                ),

                nav_panel(
                    title = "By Month",
                    layout_columns(
                        col_widths = c(12),
                        card(
                            card_header("Monthly Amortization Chart"),
                            card_body(
                                plotlyOutput("monthly_amortization_plot", height = "400px")
                            )
                        )
                    ),
                    br(),
                    card(
                        card_header("Monthly Amortization Schedule"),
                        card_body(
                            DT::dataTableOutput("monthly_amortization_table")
                        )
                    )
                )
            )
        ),

        nav_panel(
            title = "Tips & Advice",
            icon = icon("lightbulb"),
            layout_columns(
                col_widths = c(6, 6),

                card(
                    card_header("When to Buy vs Rent"),
                    card_body(
                        h4("Buying Makes Sense When:"),
                        tags$ul(
                            tags$li("You plan to stay in the area for 5+ years"),
                            tags$li("You have stable income and emergency fund"),
                            tags$li("Local rent prices are high relative to home prices"),
                            tags$li("You want to build equity and have control over your space"),
                            tags$li("There are tax benefits in your situation")
                        ),

                        h4("Renting Makes Sense When:"),
                        tags$ul(
                            tags$li("You value flexibility and mobility"),
                            tags$li("You don't want maintenance responsibilities"),
                            tags$li("Your down payment money can earn higher returns elsewhere"),
                            tags$li("Local housing market is overpriced"),
                            tags$li("You're in a transitional life phase")
                        )
                    )
                ),

                card(
                    card_header("Additional Resources"),
                    card_body(
                        h4("First-Time Buyer Programs:"),
                        tags$ul(
                            tags$li("FHA loans (3.5% down payment)"),
                            tags$li("VA loans (0% down for veterans)"),
                            tags$li("USDA loans (rural properties)"),
                            tags$li("State and local first-time buyer programs"),
                            tags$li("Down payment assistance programs")
                        ),

                        h4("Understanding PMI & HOA:"),
                        tags$ul(
                            tags$li("PMI protects the lender if you default on your loan"),
                            tags$li("PMI is automatically removed at 78% loan-to-value ratio"),
                            tags$li("You can request PMI removal at 80% loan-to-value ratio"),
                            tags$li("HOA fees cover community amenities and maintenance"),
                            tags$li("HOA fees typically increase 2-5% annually"),
                            tags$li("Review HOA financial statements and bylaws before buying")
                        ),

                        h4("Important Factors Not in Calculator:"),
                        tags$ul(
                            tags$li("Quality of schools and neighborhoods"),
                            tags$li("Commute time and transportation costs"),
                            tags$li("Lifestyle preferences and flexibility needs"),
                            tags$li("Local market trends and future development"),
                            tags$li("Personal financial stability and goals")
                        ),

                        h4("Red Flags When Buying:"),
                        tags$ul(
                            tags$li("Stretching budget to maximum pre-approval"),
                            tags$li("No emergency fund after down payment"),
                            tags$li("Unstable employment or income"),
                            tags$li("Planning to move within 2-3 years"),
                            tags$li("Local market showing signs of bubble"),
                            tags$li("High or rapidly increasing HOA fees")
                        )
                    )
                )
            )
        )
    ),

    # Theme
    theme = bs_theme(
        bootswatch = "flatly",
        primary = "#2C3E50",
        secondary = "#18BC9C",
        base_font = font_google("Open Sans")
    )
)
