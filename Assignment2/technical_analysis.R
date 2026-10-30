# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis Using R - Preliminary Stage
# Student : May Elmady
# Project : TechnicalAnalysis (GitHub)
#
# Portfolio theme: "The Toronto Cakes Portfolio"
# The stocks in portfolio.txt are companies connected to how my custom cake
# business runs every day:
#   META  - Meta Platforms: Instagram is where all Toronto Cakes orders come in
#   COST  - Costco: bulk butter, eggs, flour and sugar for big wedding orders
#   L.TO  - Loblaw (TSX): everyday baking supplies in Toronto
#   HSY   - Hershey: chocolate for ganache and chocolate fudge cakes
#   MDLZ  - Mondelez: Oreo / Cadbury toppings and fillings
#   GIS   - General Mills: flour and baking mixes
# =============================================================================


# -----------------------------------------------------------------------------
# STEP 3 - Install and load the packages needed for technical analysis
# -----------------------------------------------------------------------------
required_packages <- c("quantmod", "TTR", "xts", "zoo")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing package: ", pkg)
      install.packages(pkg, dependencies = TRUE)
    } else {
      message("Package already installed: ", pkg)
    }
  }
}

install_if_missing(required_packages)

library(quantmod)   # downloads stock data from Yahoo Finance
library(TTR)        # Technical Trading Rules (SMA, EMA, RSI ...)
library(xts)        # time-series objects used by quantmod
library(zoo)        # helper functions for ordered time series


# -----------------------------------------------------------------------------
# STEP 4 - Read the portfolio file
# -----------------------------------------------------------------------------
# Reads portfolio.txt (one symbol per line), removes blank lines, spaces and
# duplicates, and returns the symbols in upper case.
read_portfolio <- function(file_path = "portfolio.txt") {
  if (!file.exists(file_path)) {
    stop("Cannot find '", file_path, "'. Check your working directory with getwd().")
  }
  symbols <- readLines(file_path, warn = FALSE)
  symbols <- toupper(trimws(symbols))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  unique(symbols)
}


# -----------------------------------------------------------------------------
# STEP 5 - Import and load the stock data
# -----------------------------------------------------------------------------
# For every symbol in portfolio.txt, download daily prices with quantmod and
# store them as a separate data frame. Returns a named list of data frames
# (one per symbol) and also creates one data frame per stock in the
# workspace, e.g. META_df, L_TO_df.
load_stock_data <- function(file_path = "portfolio.txt",
                            from = "2025-10-01",
                            to   = "2026-10-01") {
  symbols    <- read_portfolio(file_path)
  stock_list <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    result <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result)) {
      df <- data.frame(Date = index(result), coredata(result))
      colnames(df) <- c("Date", "Open", "High", "Low", "Close",
                        "Volume", "Adjusted")
      df <- na.omit(df)

      stock_list[[sym]] <- df

      # separate data frame for each stock (L.TO becomes L_TO_df)
      df_name <- paste0(gsub("[^A-Za-z0-9]", "_", sym), "_df")
      assign(df_name, df, envir = .GlobalEnv)
      message("  Loaded ", nrow(df), " rows into ", df_name)
    }
  }

  stock_list
}


# -----------------------------------------------------------------------------
# STEP 6 - Compute basic statistics
# -----------------------------------------------------------------------------
# Mode helper: R has no built-in mode for numbers. Prices are rounded to
# 2 decimals (cents) and the most frequent value is returned.
get_mode <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)
  freq <- table(x)
  as.numeric(names(freq)[which.max(freq)])
}

# Takes one stock's data frame and calculates moving averages, mean, mode,
# median and standard deviation of the adjusted closing price.
# Returns a list with:
#   $summary - one-row data frame with all statistics
#   $data    - the original data frame with SMA_20 and SMA_50 columns added
calculate_statistics <- function(df, symbol = "STOCK",
                                 short_window = 20, long_window = 50) {
  price <- df$Adjusted

  df$SMA_20 <- SMA(price, n = short_window)   # 20-day moving average
  df$SMA_50 <- SMA(price, n = long_window)    # 50-day moving average

  summary_df <- data.frame(
    Symbol        = symbol,
    Days          = nrow(df),
    Start_Date    = min(df$Date),
    End_Date      = max(df$Date),
    Last_Close    = round(tail(price, 1), 2),
    Mean          = round(mean(price), 2),
    Median        = round(median(price), 2),
    Mode          = get_mode(price),
    Std_Dev       = round(sd(price), 2),
    Min           = round(min(price), 2),
    Max           = round(max(price), 2),
    Latest_SMA_20 = round(tail(df$SMA_20, 1), 2),
    Latest_SMA_50 = round(tail(df$SMA_50, 1), 2),
    stringsAsFactors = FALSE
  )

  list(summary = summary_df, data = df)
}

# Runs calculate_statistics() on every stock and stores the results in a
# structured format: a named list (details) plus one combined data frame.
calculate_all_statistics <- function(stock_list) {
  results <- lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], symbol = sym)
  })
  names(results) <- names(stock_list)

  summary_table <- do.call(rbind, lapply(results, function(r) r$summary))
  rownames(summary_table) <- NULL

  list(details = results, summary_table = summary_table)
}


# -----------------------------------------------------------------------------
# STEP 7 - Utilities to display the output
# -----------------------------------------------------------------------------
# 1) Show the first and last rows of a stock's data frame
display_stock_data <- function(df, symbol, n = 5) {
  cat("\n==============================================\n")
  cat(" ", symbol, " - first", n, "trading days\n")
  cat("==============================================\n")
  print(head(df, n), row.names = FALSE)
  cat("\n ", symbol, " - last", n, "trading days\n")
  print(tail(df, n), row.names = FALSE)
}

# 2) Yahoo Finance style "quote summary" for the last trading day
display_quote_summary <- function(df, symbol) {
  last_row  <- df[nrow(df), ]
  prev_row  <- df[nrow(df) - 1, ]
  change    <- last_row$Close - prev_row$Close
  change_pc <- change / prev_row$Close * 100

  cat("\n----------------------------------------------\n")
  cat(" Quote Summary:", symbol, "(", format(last_row$Date), ")\n")
  cat("----------------------------------------------\n")
  cat(sprintf(" Close           : %.2f  (%+.2f, %+.2f%%)\n",
              last_row$Close, change, change_pc))
  cat(sprintf(" Previous Close  : %.2f\n", prev_row$Close))
  cat(sprintf(" Open            : %.2f\n", last_row$Open))
  cat(sprintf(" Day's Range     : %.2f - %.2f\n", last_row$Low, last_row$High))
  cat(sprintf(" Period Range    : %.2f - %.2f\n", min(df$Low), max(df$High)))
  cat(sprintf(" Volume          : %s\n", format(last_row$Volume, big.mark = ",")))
  cat(sprintf(" Avg. Volume     : %s\n",
              format(round(mean(df$Volume)), big.mark = ",")))
}

# 3) Print the statistics table for all stocks
display_statistics_table <- function(summary_table) {
  cat("\n================ PORTFOLIO STATISTICS (Adjusted Close) ================\n")
  print(summary_table, row.names = FALSE)
}

# 4) Price chart with 20-day and 50-day moving averages (base R)
plot_price_with_ma <- function(df, symbol) {
  plot(df$Date, df$Adjusted, type = "l", lwd = 2, col = "black",
       main = paste(symbol, "- Adjusted Close with Moving Averages"),
       xlab = "Date", ylab = "Price")
  lines(df$Date, df$SMA_20, col = "blue",  lwd = 2)
  lines(df$Date, df$SMA_50, col = "red",   lwd = 2)
  legend("topleft", legend = c("Adjusted Close", "SMA 20", "SMA 50"),
         col = c("black", "blue", "red"), lwd = 2, bty = "n")
}

# 5) Candlestick chart with volume using quantmod's chartSeries
plot_candlestick <- function(df, symbol) {
  xts_data <- xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                  order.by = df$Date)
  chartSeries(xts_data, name = symbol, type = "candlesticks",
              theme = chartTheme("white"), TA = "addVo();addSMA(n=20)")
}

# 6) Bar chart comparing mean price vs standard deviation for all stocks
plot_portfolio_comparison <- function(summary_table) {
  values <- rbind(summary_table$Mean, summary_table$Std_Dev)
  barplot(values, beside = TRUE, names.arg = summary_table$Symbol,
          col = c("steelblue", "orange"),
          main = "Toronto Cakes Portfolio - Mean Price vs Std. Deviation",
          ylab = "Price")
  legend("topright", legend = c("Mean", "Std. Deviation"),
         fill = c("steelblue", "orange"), bty = "n")
}


# =============================================================================
# MAIN - run the whole process
# =============================================================================
# Set the working directory to the Assignment2 folder of your repository, e.g.
# setwd("C:/Users/May/Documents/GitHub/TechnicalAnalysis/Assignment2")

portfolio <- read_portfolio("portfolio.txt")
cat("Symbols in portfolio:", paste(portfolio, collapse = ", "), "\n")

stocks <- load_stock_data("portfolio.txt", from = "2025-10-01", to = "2026-10-01")

stats <- calculate_all_statistics(stocks)

for (sym in names(stocks)) {
  display_stock_data(stocks[[sym]], sym)
  display_quote_summary(stocks[[sym]], sym)
}

display_statistics_table(stats$summary_table)

for (sym in names(stats$details)) {
  plot_price_with_ma(stats$details[[sym]]$data, sym)
  plot_candlestick(stocks[[sym]], sym)
}

plot_portfolio_comparison(stats$summary_table)
