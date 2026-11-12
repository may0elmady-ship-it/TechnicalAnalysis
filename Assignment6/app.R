# =============================================================================
#  BDA400 - Data Science Tools and Techniques
#  Assignment 6: Technical Analysis using R, Visualization Phase
#  Student : May Elmady
#
#  Project : "Toronto Cakes Portfolio Dashboard"
#            An R Shiny dashboard that fetches stock data from Yahoo Finance,
#            draws it as a line / candlestick / area chart, overlays technical
#            indicators (Moving Averages, Bollinger Bands, RSI, MACD, Volume)
#            that can be switched on and off, and annotates the bars with
#            Buy / Sell / Hold signals produced by customizable trading rules.
#
#  Why these stocks? I run Toronto Cakes, a custom cake business that takes
#  all of its orders through Instagram. This dashboard builds on Assignment 2
#  (same repository, folder Assignment2): it uses the same "Toronto Cakes
#  Portfolio" and the same 20/50-day moving averages, now inside Shiny.
#  The watch-list follows the companies my business actually depends on:
#     META  - Meta Platforms (owner of Instagram, where my orders come in)
#     COST  - Costco (bulk butter, eggs, flour and sugar for big orders)
#     HSY   - The Hershey Company (chocolate)
#     MDLZ  - Mondelez International (Oreo / Cadbury cake toppings)
#     GIS   - General Mills (flour and baking mixes)
#     L.TO  - Loblaw Companies (Canadian grocery chain, listed in Toronto)
#
#  How to run: open this file in RStudio and click "Run App"
#              (or run shiny::runApp() from the project folder).
#
#  The file is organised in the same order as the assignment instructions:
#     STEP 1 - Data Collection and Setup
#     STEP 2 - Visualizing Stock Data
#     STEP 3 - Overlay Technical Indicators
#     STEP 4 - Implement Trading Rules and Annotations
#     STEP 5 - Shiny UI and Server (puts steps 1-4 together)
# =============================================================================


# =============================================================================
# STEP 1 - DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1.1 Choose a data source -----------------------------------------------
# Primary source  : Yahoo Finance, through quantmod::getSymbols(src = "yahoo").
# Second source   : a CSV file uploaded by the user (same columns as a Yahoo
#                   Finance "Download" file: Date, Open, High, Low, Close,
#                   Volume). This is a fallback if Yahoo is unreachable.

# ---- 1.2 Install and load the necessary packages ----------------------------
# The instructions use:
#   install.packages("shiny"); install.packages("ggplot2"); install.packages("quantmod")
# I also use 'patchwork' (to stack the RSI / MACD / Volume panels under the
# price chart) and 'scales' (axis labels). The code below installs any
# package that is missing, then loads all of them.
required_packages <- c("shiny", "ggplot2", "quantmod", "patchwork", "scales")
missing_packages  <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(ggplot2)
library(quantmod)   # also loads TTR (SMA, EMA, RSI, MACD, BBands), xts and zoo
library(patchwork)
library(scales)

# ---- 1.3 Fetch historical stock data ----------------------------------------
# Default values (the dashboard lets the user change all of them).
stock_symbol <- "META"                  # Instagram's parent company
start_date   <- Sys.Date() - 365        # one year of history
end_date     <- Sys.Date()

# Watch-list shown in the stock picker (the user can also type any symbol).
portfolio_choices <- c(
  "META - Meta Platforms (Instagram)"   = "META",
  "COST - Costco (bulk baking supplies)" = "COST",
  "HSY - Hershey (chocolate)"           = "HSY",
  "MDLZ - Mondelez (Oreo / Cadbury)"    = "MDLZ",
  "GIS - General Mills (flour, mixes)"  = "GIS",
  "L.TO - Loblaw (Canadian grocery)"    = "L.TO"
)

# clean_ohlc(): keeps Open/High/Low/Close/Volume with standard names and
# handles missing data (missing prices are carried forward from the previous
# day; rows that are still empty are removed).
clean_ohlc <- function(x) {
  volume <- if (has.Vo(x)) Vo(x) else xts(rep(0, nrow(x)), order.by = index(x))
  x <- cbind(Op(x), Hi(x), Lo(x), Cl(x), volume)
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")

  x <- na.locf(x, na.rm = FALSE)                       # fill gaps (holidays, bad ticks)
  x <- x[complete.cases(x[, c("Open", "High", "Low", "Close")]), ]
  x <- x[x$Close > 0, ]                                 # remove impossible prices
  x$Volume[is.na(x$Volume)] <- 0

  if (nrow(x) == 0) stop("The data source returned no usable price rows.", call. = FALSE)
  x
}

# fetch_stock_data(): downloads prices from Yahoo Finance with error handling.
# 'warmup_days' downloads extra history BEFORE the start date so that long
# indicators (for example a 50-day moving average) already have a value on
# the first day the user asked to see.
fetch_stock_data <- function(symbol, from, to, warmup_days = 400) {
  symbol <- toupper(trimws(symbol))
  from   <- as.Date(from)
  to     <- as.Date(to)
  if (!nzchar(symbol)) stop("Please enter a stock symbol.", call. = FALSE)
  if (from >= to)      stop("The start date must be before the end date.", call. = FALSE)

  data <- tryCatch(
    suppressWarnings(
      getSymbols(symbol, src = "yahoo",
                 from = from - warmup_days,
                 to   = to + 1,            # 'to' is exclusive in getSymbols
                 auto.assign = FALSE)
    ),
    error = function(e) {
      stop(sprintf(paste0("Could not download '%s' from Yahoo Finance. ",
                          "Check the symbol and your internet connection, ",
                          "or switch the data source to 'Upload CSV'. (Details: %s)"),
                   symbol, conditionMessage(e)), call. = FALSE)
    }
  )
  clean_ohlc(data)
}

# read_stock_csv(): second data source - reads a CSV in Yahoo Finance format.
read_stock_csv <- function(path) {
  raw <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) stop("The CSV file could not be read.", call. = FALSE))
  names(raw) <- trimws(names(raw))
  find_col <- function(name) {
    hit <- which(tolower(names(raw)) == tolower(name))
    if (length(hit) == 0) NULL else raw[[hit[1]]]
  }

  dates <- find_col("Date")
  close <- find_col("Close")
  if (is.null(dates) || is.null(close)) {
    stop("The CSV needs at least a 'Date' and a 'Close' column.", call. = FALSE)
  }
  to_num <- function(v) suppressWarnings(as.numeric(gsub("[$,]", "", v)))
  close  <- to_num(close)
  open   <- if (is.null(find_col("Open")))   close else to_num(find_col("Open"))
  high   <- if (is.null(find_col("High")))   pmax(open, close) else to_num(find_col("High"))
  low    <- if (is.null(find_col("Low")))    pmin(open, close) else to_num(find_col("Low"))
  volume <- if (is.null(find_col("Volume"))) rep(0, length(close)) else to_num(find_col("Volume"))

  parsed_dates <- as.Date(dates, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%b %d, %Y"),
                          optional = TRUE)
  keep <- !is.na(parsed_dates)
  if (!any(keep)) stop("No valid dates were found in the CSV 'Date' column.", call. = FALSE)

  x <- xts(cbind(Open = open, High = high, Low = low, Close = close, Volume = volume)[keep, , drop = FALSE],
           order.by = parsed_dates[keep])
  x <- x[!duplicated(index(x)), ]
  clean_ohlc(x)
}

# Quick check that runs once when the app starts (prints the first rows in
# the R console). If there is no internet the app still starts normally.
stock_data <- tryCatch(
  fetch_stock_data(stock_symbol, start_date, end_date, warmup_days = 0),
  error = function(e) { message("Start-up download skipped: ", conditionMessage(e)); NULL }
)
if (!is.null(stock_data)) print(head(stock_data))


# =============================================================================
# STEP 2 - VISUALIZING STOCK DATA
# =============================================================================

# Colours used throughout the dashboard (Toronto Cakes berry & cream palette).
col_brand <- "#7B2D4B"   # berry  - price line / area
col_up    <- "#2E8B57"   # green  - rising bars and Buy signals
col_down  <- "#C0392B"   # red    - falling bars and Sell signals
col_hold  <- "#8C8C8C"   # grey   - Hold
col_short <- "#E67E22"   # orange - short moving average
col_long  <- "#2C6FBB"   # blue   - long moving average

# apply_time_frame(): turns daily bars into weekly or monthly bars.
apply_time_frame <- function(x, time_frame) {
  out <- switch(time_frame,
    "Daily"   = x,
    "Weekly"  = to.weekly(x,  indexAt = "endof", OHLC = TRUE),
    "Monthly" = to.monthly(x, indexAt = "endof", OHLC = TRUE)
  )
  colnames(out) <- c("Open", "High", "Low", "Close", "Volume")
  out
}

# xts_to_df(): ggplot2 works with data frames, so convert the xts object.
xts_to_df <- function(x) {
  data.frame(Date = as.Date(index(x)), coredata(x), row.names = NULL)
}

# bar_width(): width (in days) of a candle for each time frame.
bar_width <- function(time_frame) {
  switch(time_frame, "Daily" = 0.7, "Weekly" = 4.5, "Monthly" = 20)
}

# base_price_chart(): draws the price as a Line, Candlestick or Area chart.
base_price_chart <- function(df, chart_type, time_frame) {
  p <- ggplot(df, aes(x = Date))

  if (chart_type == "Candlestick") {
    w    <- bar_width(time_frame)
    up   <- df[df$Close >= df$Open, ]
    down <- df[df$Close <  df$Open, ]
    p <- p +
      geom_segment(data = up,   aes(xend = Date, y = Low, yend = High), colour = col_up) +
      geom_segment(data = down, aes(xend = Date, y = Low, yend = High), colour = col_down) +
      geom_rect(data = up,   aes(xmin = Date - w / 2, xmax = Date + w / 2,
                                 ymin = Open, ymax = Close),
                fill = col_up, colour = col_up, linewidth = 0.2) +
      geom_rect(data = down, aes(xmin = Date - w / 2, xmax = Date + w / 2,
                                 ymin = Close, ymax = Open),
                fill = col_down, colour = col_down, linewidth = 0.2)

  } else if (chart_type == "Area") {
    floor_value <- min(df$Low, na.rm = TRUE) * 0.97
    p <- p +
      geom_ribbon(aes(ymin = floor_value, ymax = Close), fill = col_brand, alpha = 0.18) +
      geom_line(aes(y = Close), colour = col_brand, linewidth = 0.8)

  } else {  # "Line"
    p <- p + geom_line(aes(y = Close), colour = col_brand, linewidth = 0.8)
  }

  p + labs(x = NULL, y = "Price")
}


# =============================================================================
# STEP 3 - OVERLAY TECHNICAL INDICATORS
# =============================================================================
# The indicators from the earlier part of the project are calculated with the
# TTR package (installed with quantmod). They are ALWAYS calculated (the
# trading rules need them) but only DRAWN when the user ticks them in the
# "Technical indicators" checkboxes - this is the on/off toggle.
#   - Moving Averages and Bollinger Bands are drawn directly on the price chart.
#   - RSI (0-100 scale), MACD and Volume have different units, so they are
#     drawn as extra panels underneath the price chart, sharing the same
#     date axis.

add_indicators <- function(df, p) {
  n      <- nrow(df)
  safe   <- function(expr) tryCatch(as.numeric(expr), error = function(e) rep(NA_real_, n))
  ma_fun <- if (p$ma_type == "EMA") EMA else SMA

  # Moving averages (short-term and long-term)
  df$MA_Short <- safe(ma_fun(df$Close, n = p$short_n))
  df$MA_Long  <- safe(ma_fun(df$Close, n = p$long_n))

  # Bollinger Bands
  bb <- tryCatch(BBands(df[, c("High", "Low", "Close")], n = p$bb_n, sd = p$bb_sd),
                 error = function(e) NULL)
  df$BB_Upper <- if (is.null(bb)) NA_real_ else as.numeric(bb[, "up"])
  df$BB_Mid   <- if (is.null(bb)) NA_real_ else as.numeric(bb[, "mavg"])
  df$BB_Lower <- if (is.null(bb)) NA_real_ else as.numeric(bb[, "dn"])

  # RSI
  df$RSI <- safe(RSI(df$Close, n = p$rsi_n))

  # MACD (MACD line, signal line and histogram)
  macd <- tryCatch(MACD(df$Close, nFast = p$macd_fast, nSlow = p$macd_slow,
                        nSig = p$macd_signal, maType = "EMA", percent = FALSE),
                   error = function(e) NULL)
  df$MACD        <- if (is.null(macd)) NA_real_ else as.numeric(macd[, "macd"])
  df$MACD_Signal <- if (is.null(macd)) NA_real_ else as.numeric(macd[, "signal"])
  df$MACD_Hist   <- df$MACD - df$MACD_Signal
  df
}

# Overlay layers that go ON the price chart --------------------------------
add_ma_layers <- function(plot, df, p) {
  short_lab <- sprintf("%s %d", p$ma_type, p$short_n)
  long_lab  <- sprintf("%s %d", p$ma_type, p$long_n)
  ma_long_df <- rbind(
    data.frame(Date = df$Date, Value = df$MA_Short, Series = short_lab),
    data.frame(Date = df$Date, Value = df$MA_Long,  Series = long_lab)
  )
  ma_long_df$Series <- factor(ma_long_df$Series, levels = c(short_lab, long_lab))
  plot +
    geom_line(data = ma_long_df[!is.na(ma_long_df$Value), ],
              aes(x = Date, y = Value, colour = Series), linewidth = 0.8) +
    scale_colour_manual(values = setNames(c(col_short, col_long), c(short_lab, long_lab)),
                        name = NULL)
}

add_bbands_layers <- function(plot, df) {
  bb <- df[!is.na(df$BB_Upper), ]
  if (nrow(bb) == 0) return(plot)
  plot +
    geom_ribbon(data = bb, aes(x = Date, ymin = BB_Lower, ymax = BB_Upper),
                fill = "#9B7BB8", alpha = 0.12, inherit.aes = FALSE) +
    geom_line(data = bb, aes(x = Date, y = BB_Upper), colour = "#9B7BB8",
              linetype = "dashed", linewidth = 0.4) +
    geom_line(data = bb, aes(x = Date, y = BB_Lower), colour = "#9B7BB8",
              linetype = "dashed", linewidth = 0.4)
}

# Indicator panels that go UNDER the price chart ---------------------------
rsi_panel <- function(df, p, x_limits) {
  ggplot(df[!is.na(df$RSI), ], aes(x = Date, y = RSI)) +
    annotate("rect", xmin = x_limits[1], xmax = x_limits[2],
             ymin = p$rsi_low, ymax = p$rsi_high, fill = "#F3E6EA", alpha = 0.6) +
    geom_hline(yintercept = c(p$rsi_low, p$rsi_high), linetype = "dashed", colour = col_hold) +
    geom_line(colour = "#6A4C93", linewidth = 0.7) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, p$rsi_low, p$rsi_high, 100)) +
    labs(x = NULL, y = sprintf("RSI (%d)", p$rsi_n))
}

macd_panel <- function(df, p, w = 0.8) {
  d <- df[!is.na(df$MACD), ]
  ggplot(d, aes(x = Date)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_col(aes(y = MACD_Hist, fill = MACD_Hist >= 0), width = w, alpha = 0.6,
             show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = col_up, "FALSE" = col_down)) +
    geom_line(aes(y = MACD), colour = col_long, linewidth = 0.7) +
    geom_line(aes(y = MACD_Signal), colour = col_short, linewidth = 0.7) +
    labs(x = NULL, y = sprintf("MACD (%d,%d,%d)", p$macd_fast, p$macd_slow, p$macd_signal))
}

volume_panel <- function(df) {
  ggplot(df, aes(x = Date, y = Volume, fill = Close >= Open)) +
    geom_col(alpha = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = col_up, "FALSE" = col_down)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = "Volume")
}


# =============================================================================
# STEP 4 - IMPLEMENT TRADING RULES AND ANNOTATIONS
# =============================================================================
# Four rule sets are available; every number they use (MA lengths, MA type,
# RSI levels, MACD settings) can be changed in the sidebar.
#
#  1) Moving Average Crossover  (the rule given in the instructions)
#       - short MA crosses ABOVE long MA -> Buy
#       - short MA crosses BELOW long MA -> Sell
#       - otherwise                      -> Hold
#  2) RSI Overbought / Oversold
#       - RSI crosses back UP through the oversold level   -> Buy
#       - RSI crosses back DOWN through the overbought level -> Sell
#  3) MACD Signal-line Crossover
#       - MACD crosses ABOVE its signal line -> Buy
#       - MACD crosses BELOW its signal line -> Sell
#  4) Combined (momentum + price trend + RSI filter)
#       - Buy  when MACD crosses above its signal line, the closing price is
#              above the short MA (confirmed up-move) AND RSI is below the
#              overbought level (not too expensive)
#       - Sell when MACD crosses below its signal line with the price below
#              the short MA, OR RSI falls back below the overbought level
#
# Two kinds of signal are produced:
#   * Regime  - a Buy / Sell / Hold label on EVERY bar (same idea as the
#               ifelse() example in the instructions).
#   * Signal  - the actual trade events (the bar where a crossover happens).
#               With "one open position" switched on, a Buy is only kept when
#               we are not already holding the stock, and a Sell only when we
#               are - so the chart shows a clean Buy -> Sell -> Buy sequence.

# Helper: TRUE on the bar where series a crosses above / below series b.
lag1       <- function(v) c(NA, head(v, -1))
cross_up   <- function(a, b) { r <- lag1(a) <= lag1(b) & a > b; r[is.na(r)] <- FALSE; r }
cross_down <- function(a, b) { r <- lag1(a) >= lag1(b) & a < b; r[is.na(r)] <- FALSE; r }

generate_signals <- function(df, strategy, p) {
  n <- nrow(df)

  if (strategy == "Moving Average Crossover") {
    regime   <- ifelse(df$MA_Short > df$MA_Long, "Buy",
                ifelse(df$MA_Short < df$MA_Long, "Sell", "Hold"))
    buy_evt  <- cross_up(df$MA_Short, df$MA_Long)
    sell_evt <- cross_down(df$MA_Short, df$MA_Long)

  } else if (strategy == "RSI Overbought / Oversold") {
    regime   <- ifelse(df$RSI < p$rsi_low, "Buy",
                ifelse(df$RSI > p$rsi_high, "Sell", "Hold"))
    buy_evt  <- cross_up(df$RSI, rep(p$rsi_low, n))
    sell_evt <- cross_down(df$RSI, rep(p$rsi_high, n))

  } else if (strategy == "MACD Signal Crossover") {
    regime   <- ifelse(df$MACD > df$MACD_Signal, "Buy",
                ifelse(df$MACD < df$MACD_Signal, "Sell", "Hold"))
    buy_evt  <- cross_up(df$MACD, df$MACD_Signal)
    sell_evt <- cross_down(df$MACD, df$MACD_Signal)

  } else {  # "Combined (MACD + price trend + RSI)"
    above    <- df$Close > df$MA_Short          # price above the short-term MA
    regime   <- ifelse(df$MACD > df$MACD_Signal & above & df$RSI < p$rsi_high, "Buy",
                ifelse(df$MACD < df$MACD_Signal & !above, "Sell", "Hold"))
    buy_evt  <- cross_up(df$MACD, df$MACD_Signal) & above & df$RSI < p$rsi_high
    sell_evt <- (cross_down(df$MACD, df$MACD_Signal) & !above) |
                cross_down(df$RSI, rep(p$rsi_high, n))
    buy_evt[is.na(buy_evt)]   <- FALSE
    sell_evt[is.na(sell_evt)] <- FALSE
  }

  regime[is.na(regime)] <- "Hold"     # not enough history yet -> Hold
  df$Regime    <- regime
  df$Raw_Event <- ifelse(buy_evt, "Buy", ifelse(sell_evt, "Sell", NA_character_))
  df
}

# keep_one_position(): removes repeated Buys (while already holding) and
# Sells (while not holding). Starts the period with no position.
keep_one_position <- function(events) {
  holding <- FALSE
  out <- rep(NA_character_, length(events))
  for (i in seq_along(events)) {
    e <- events[i]
    if (is.na(e)) next
    if (e == "Buy"  && !holding) { out[i] <- "Buy";  holding <- TRUE  }
    if (e == "Sell" &&  holding) { out[i] <- "Sell"; holding <- FALSE }
  }
  out
}

# strategy_performance(): compares following the signals with simply buying
# and holding. A signal on a bar is acted on at that bar's close, so the
# position earns the return from the NEXT bar onward (no look-ahead).
strategy_performance <- function(df) {
  trades  <- keep_one_position(df$Raw_Event)
  holding <- 0
  pos     <- numeric(nrow(df))
  for (i in seq_len(nrow(df))) {
    pos[i] <- holding
    if (!is.na(trades[i]) && trades[i] == "Buy")  holding <- 1
    if (!is.na(trades[i]) && trades[i] == "Sell") holding <- 0
  }
  ret <- c(0, diff(df$Close) / head(df$Close, -1))
  list(strategy = prod(1 + pos * ret) - 1,
       buy_hold = tail(df$Close, 1) / df$Close[1] - 1,
       trades   = sum(!is.na(trades)))
}

# add_signal_annotations(): writes the signals on the bars of the chart.
add_signal_annotations <- function(plot, df, mode) {
  if (mode == "none") return(plot)
  offset <- 0.035 * diff(range(c(df$Low, df$High), na.rm = TRUE))

  if (mode == "all") {
    # Every bar gets its regime label (as in the instructions' geom_text example)
    for (s in c("Buy", "Sell", "Hold")) {
      d <- df[df$Regime == s, ]
      if (nrow(d) == 0) next
      plot <- plot + geom_text(data = d, aes(x = Date, y = Low - offset, label = Regime),
                               colour = c(Buy = col_up, Sell = col_down, Hold = col_hold)[[s]],
                               size = 2.3, angle = 90, hjust = 1, fontface = "bold")
    }
    return(plot)
  }

  # mode == "events": arrow + label only where a trade signal happens
  buys  <- df[!is.na(df$Signal) & df$Signal == "Buy", ]
  sells <- df[!is.na(df$Signal) & df$Signal == "Sell", ]
  if (nrow(buys) > 0) {
    plot <- plot +
      geom_point(data = buys, aes(x = Date, y = Low - offset * 0.6),
                 shape = 24, size = 3, fill = col_up, colour = "white") +
      geom_label(data = buys, aes(x = Date, y = Low - offset * 1.9, label = "BUY"),
                 fill = col_up, colour = "white", size = 2.6, fontface = "bold",
                 label.size = 0, label.padding = unit(0.12, "lines"))
  }
  if (nrow(sells) > 0) {
    plot <- plot +
      geom_point(data = sells, aes(x = Date, y = High + offset * 0.6),
                 shape = 25, size = 3, fill = col_down, colour = "white") +
      geom_label(data = sells, aes(x = Date, y = High + offset * 1.9, label = "SELL"),
                 fill = col_down, colour = "white", size = 2.6, fontface = "bold",
                 label.size = 0, label.padding = unit(0.12, "lines"))
  }
  plot
}

# build_dashboard_chart(): puts the price chart, overlays, annotations and
# lower indicator panels together into one figure.
build_dashboard_chart <- function(df, s) {
  indicators <- s$technical_indicators
  w        <- bar_width(s$time_frame)
  x_limits <- range(df$Date) + c(-w, w)
  theme_dash <- theme_minimal(base_size = 13) +
    theme(legend.position = "top", legend.justification = "left",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", colour = col_brand))

  # Price chart
  p <- base_price_chart(df, s$chart_type, s$time_frame)
  if ("Bollinger Bands" %in% indicators) p <- add_bbands_layers(p, df)
  if ("Moving Averages" %in% indicators) p <- add_ma_layers(p, df, s$params)
  p <- add_signal_annotations(p, df, s$annotation_mode)
  p <- p +
    scale_x_date(limits = x_limits, expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = label_dollar()) +
    labs(title = sprintf("%s  |  %s  |  %s chart", s$symbol, s$time_frame, s$chart_type),
         subtitle = sprintf("Trading rule: %s", s$strategy_label)) +
    theme_dash

  # Lower panels - only the ones the user switched on
  panels  <- list(p)
  heights <- c(3)
  if ("Volume" %in% indicators) {
    panels  <- c(panels, list(volume_panel(df) + scale_x_date(limits = x_limits, expand = expansion(mult = 0.01)) + theme_dash))
    heights <- c(heights, 0.8)
  }
  if ("RSI" %in% indicators) {
    panels  <- c(panels, list(rsi_panel(df, s$params, x_limits) + scale_x_date(limits = x_limits, expand = expansion(mult = 0.01)) + theme_dash))
    heights <- c(heights, 1)
  }
  if ("MACD" %in% indicators) {
    panels  <- c(panels, list(macd_panel(df, s$params, w) + scale_x_date(limits = x_limits, expand = expansion(mult = 0.01)) + theme_dash))
    heights <- c(heights, 1)
  }
  if (length(panels) == 1) return(p)
  wrap_plots(panels, ncol = 1, heights = heights)
}


# =============================================================================
# STEP 5 - SHINY APP: USER INTERFACE
# =============================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#FBF7F4; }
    .app-title { color:#7B2D4B; font-weight:700; margin-bottom:0; }
    .app-sub   { color:#8C6E78; margin-top:2px; }
    .well { background:#FFFFFF; border:1px solid #EAD9DF; }
    .well h4 { color:#7B2D4B; font-weight:700; margin-top:18px; }
    .cards { display:flex; flex-wrap:wrap; gap:10px; margin:6px 0 12px; }
    .card  { background:#FFFFFF; border:1px solid #EAD9DF; border-radius:10px;
             padding:10px 14px; min-width:150px; flex:1; }
    .card .lbl { font-size:12px; color:#8C6E78; text-transform:uppercase; letter-spacing:.04em; }
    .card .val { font-size:20px; font-weight:700; color:#333; }
    .buy { color:#2E8B57 !important; } .sell { color:#C0392B !important; } .hold { color:#8C8C8C !important; }
    .warn { background:#FFF4E0; border:1px solid #F0C36D; border-radius:8px; padding:8px 12px; margin-bottom:10px; }
    details summary { cursor:pointer; color:#7B2D4B; font-weight:600; margin:6px 0; }
  "))),

  h2("Toronto Cakes Portfolio Dashboard", class = "app-title"),
  p("BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase  |  May Elmady",
    class = "app-sub"),

  sidebarLayout(
    sidebarPanel(width = 3,

      # ---- Step 1 widgets: data -------------------------------------------
      h4("1. Data"),
      radioButtons("data_source", "Data source:",
                   choices = c("Yahoo Finance" = "yahoo", "Upload CSV" = "csv"), inline = TRUE),
      conditionalPanel("input.data_source == 'yahoo'",
        selectizeInput("stock_symbol", "Stock symbol:", choices = portfolio_choices,
                       selected = stock_symbol, options = list(create = TRUE)),
        helpText("Pick from my watch-list or type any Yahoo Finance symbol.")
      ),
      conditionalPanel("input.data_source == 'csv'",
        fileInput("csv_file", "Upload a price CSV:", accept = ".csv"),
        helpText("Columns: Date, Open, High, Low, Close, Volume (Yahoo Finance download format).")
      ),

      # ---- Step 2 widgets: date range, time frame, chart type ----------------
      h4("2. Chart"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date, max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      radioButtons("chart_type", "Chart type:",
                   choices = c("Candlestick", "Line", "Area"), inline = TRUE),

      # ---- Step 3 widgets: indicator on/off + settings -------------------------
      h4("3. Technical indicators"),
      checkboxGroupInput("technical_indicators", "Show on chart:",
                         choices  = c("Moving Averages", "Bollinger Bands", "RSI", "MACD", "Volume"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      tags$details(open = NA,
        tags$summary("Indicator settings"),
        radioButtons("ma_type", "Moving average type:", c("SMA", "EMA"), inline = TRUE),
        sliderInput("short_n", "Short-term MA period:", min = 5,  max = 100, value = 20),
        sliderInput("long_n",  "Long-term MA period:",  min = 10, max = 200, value = 50),
        sliderInput("rsi_n", "RSI period:", min = 5, max = 30, value = 14),
        sliderInput("rsi_levels", "RSI oversold / overbought:", min = 10, max = 90, value = c(30, 70)),
        fluidRow(
          column(4, numericInput("macd_fast",   "MACD fast",   12, min = 2, max = 50)),
          column(4, numericInput("macd_slow",   "slow",        26, min = 3, max = 100)),
          column(4, numericInput("macd_signal", "signal",       9, min = 2, max = 50))
        ),
        fluidRow(
          column(6, numericInput("bb_n",  "Bollinger period", 20, min = 5, max = 100)),
          column(6, numericInput("bb_sd", "Std. dev.",         2, min = 1, max = 4, step = 0.5))
        )
      ),

      # ---- Step 4 widgets: trading rules + annotations -------------------------
      h4("4. Trading rules"),
      selectInput("strategy", "Strategy:",
                  choices = c("Moving Average Crossover", "RSI Overbought / Oversold",
                              "MACD Signal Crossover", "Combined (MACD + price trend + RSI)")),
      radioButtons("annotation_mode", "Annotate the bars with:",
                   choices = c("Buy / Sell signals"           = "events",
                               "Every bar (Buy / Sell / Hold)" = "all",
                               "No annotations"                = "none")),
      checkboxInput("position_aware", "One open position at a time (Buy -> Sell -> Buy)", TRUE)
    ),

    mainPanel(width = 9,
      uiOutput("data_warnings"),
      uiOutput("summary_cards"),
      tabsetPanel(
        tabPanel("Chart", br(), plotOutput("stock_chart", height = "780px")),
        tabPanel("Signals",
          br(),
          downloadButton("download_signals", "Download all bars + signals (CSV)"),
          br(), br(),
          p("Bars where the selected trading rule produced a Buy or Sell signal:"),
          tableOutput("signal_table")
        ),
        tabPanel("Portfolio",
          br(),
          fluidRow(
            column(8, selectizeInput("portfolio_symbols", "Portfolio stocks:",
                                     choices = portfolio_choices, selected = unname(portfolio_choices),
                                     multiple = TRUE, options = list(create = TRUE))),
            column(4, br(), actionButton("load_portfolio", "Load / refresh portfolio",
                                         class = "btn-primary"))
          ),
          helpText("Each stock is rescaled to 100 on the first day of the selected date range, so the lines show relative performance."),
          plotOutput("portfolio_chart", height = "420px"),
          tableOutput("portfolio_table")
        ),
        tabPanel("About the rules",
          br(),
          h4("How to read the chart"),
          tags$ul(
            tags$li("Green triangle + BUY label under a bar: the selected rule says buy at that bar's close."),
            tags$li("Red triangle + SELL label above a bar: the selected rule says sell."),
            tags$li("'Every bar' mode writes Buy / Sell / Hold on each bar, following the ifelse() rule in the instructions."),
            tags$li("Indicators can be switched on and off; their settings and the rules update the chart instantly.")
          ),
          h4("Trading rules"),
          tags$ol(
            tags$li(strong("Moving Average Crossover: "), "Buy when the short MA crosses above the long MA; Sell when it crosses below; otherwise Hold."),
            tags$li(strong("RSI Overbought / Oversold: "), "Buy when RSI climbs back above the oversold level; Sell when it drops back below the overbought level."),
            tags$li(strong("MACD Signal Crossover: "), "Buy when MACD crosses above its signal line; Sell when it crosses below."),
            tags$li(strong("Combined: "), "Buy on a MACD bullish crossover only when the price is above the short MA and RSI is not overbought; Sell on a MACD bearish crossover with the price below the short MA, or when RSI falls back from overbought.")
          ),
          p(em("For learning purposes only - not investment advice."))
        )
      )
    )
  )
)


# =============================================================================
# STEP 5 - SHINY APP: SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ---- Step 1: fetch the data (Yahoo or CSV) with error handling ------------
  raw_data <- reactive({
    req(input$date_range)
    if (input$data_source == "csv") {
      req(input$csv_file)
      tryCatch(read_stock_csv(input$csv_file$datapath),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    } else {
      req(input$stock_symbol)
      tryCatch(fetch_stock_data(input$stock_symbol, input$date_range[1], input$date_range[2]),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    }
  })

  # All indicator and rule settings in one list
  params <- reactive({
    list(ma_type = input$ma_type, short_n = input$short_n, long_n = input$long_n,
         rsi_n = input$rsi_n, rsi_low = input$rsi_levels[1], rsi_high = input$rsi_levels[2],
         macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal,
         bb_n = input$bb_n, bb_sd = input$bb_sd)
  })

  symbol_label <- reactive({
    if (input$data_source == "csv") {
      if (is.null(input$csv_file)) "CSV" else sub("\\.csv$", "", input$csv_file$name, ignore.case = TRUE)
    } else toupper(input$stock_symbol)
  })

  # ---- Steps 2-4: time frame -> indicators -> signals -> date filter ---------
  analysis <- reactive({
    p <- params()
    validate(
      need(input$date_range[1] < input$date_range[2], "The start date must be before the end date."),
      need(p$short_n < p$long_n, "The short-term MA period must be smaller than the long-term period."),
      need(p$rsi_low < p$rsi_high, "The RSI oversold level must be below the overbought level."),
      need(all(!is.na(c(p$macd_fast, p$macd_slow, p$macd_signal, p$bb_n, p$bb_sd))),
           "Please fill in all indicator settings."),
      need(p$macd_fast < p$macd_slow, "The MACD fast period must be smaller than the slow period.")
    )

    x  <- apply_time_frame(raw_data(), input$time_frame)
    df <- xts_to_df(x)
    df <- add_indicators(df, p)                    # on full history (incl. warm-up)
    df <- generate_signals(df, input$strategy, p)

    # Filter to the date range the user selected
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 2, "No price data in the selected date range / time frame."))

    df$Signal <- if (isTRUE(input$position_aware)) keep_one_position(df$Raw_Event) else df$Raw_Event
    df
  })

  # ---- Warnings when there is not enough history for an indicator -----------
  output$data_warnings <- renderUI({
    df <- analysis()
    msgs <- c()
    if (all(is.na(df$MA_Long)))
      msgs <- c(msgs, sprintf("Not enough %s bars to calculate the %d-period moving average. Widen the date range, use a shorter time frame, or lower the long MA period.",
                              tolower(input$time_frame), input$long_n))
    if (all(is.na(df$MACD)))
      msgs <- c(msgs, "Not enough bars to calculate MACD for this time frame.")
    if (length(msgs) == 0) return(NULL)
    div(class = "warn", HTML(paste(msgs, collapse = "<br>")))
  })

  # ---- Summary cards --------------------------------------------------------
  output$summary_cards <- renderUI({
    df   <- analysis()
    perf <- strategy_performance(df)
    last_evt <- which(!is.na(df$Signal))
    last_txt <- if (length(last_evt) == 0) "None" else
      sprintf("%s (%s)", df$Signal[max(last_evt)], format(df$Date[max(last_evt)], "%b %d, %Y"))
    last_cls <- if (length(last_evt) == 0) "hold" else tolower(df$Signal[max(last_evt)])
    regime   <- tail(df$Regime, 1)
    pct      <- function(v) sprintf("%+.1f%%", 100 * v)
    card     <- function(lbl, val, cls = "") div(class = "card", div(class = "lbl", lbl), div(class = paste("val", cls), val))

    div(class = "cards",
      card("Last close", dollar(tail(df$Close, 1))),
      card("Change in range", pct(perf$buy_hold), if (perf$buy_hold >= 0) "buy" else "sell"),
      card("Current rule state", regime, tolower(regime)),
      card("Latest signal", last_txt, last_cls),
      card("Buy / Sell signals", sprintf("%d / %d", sum(df$Signal == "Buy", na.rm = TRUE),
                                          sum(df$Signal == "Sell", na.rm = TRUE))),
      card("Rule vs buy & hold", sprintf("%s vs %s", pct(perf$strategy), pct(perf$buy_hold)))
    )
  })

  # ---- Main chart (Steps 2, 3 and 4 combined) -------------------------------
  output$stock_chart <- renderPlot({
    df <- analysis()
    p  <- params()
    strategy_label <- switch(input$strategy,
      "Moving Average Crossover"         = sprintf("MA Crossover (%s %d / %d)", p$ma_type, p$short_n, p$long_n),
      "RSI Overbought / Oversold"        = sprintf("RSI(%d) %d / %d", p$rsi_n, p$rsi_low, p$rsi_high),
      "MACD Signal Crossover"            = sprintf("MACD(%d,%d,%d) signal crossover", p$macd_fast, p$macd_slow, p$macd_signal),
      sprintf("Combined: MACD(%d,%d,%d) cross + price vs %s %d + RSI < %d",
              p$macd_fast, p$macd_slow, p$macd_signal, p$ma_type, p$short_n, p$rsi_high))

    build_dashboard_chart(df, list(
      symbol = symbol_label(), time_frame = input$time_frame, chart_type = input$chart_type,
      technical_indicators = input$technical_indicators, annotation_mode = input$annotation_mode,
      strategy_label = strategy_label, params = p))
  }, res = 96)

  # ---- Signals table and download -------------------------------------------
  output$signal_table <- renderTable({
    df <- analysis()
    ev <- df[!is.na(df$Signal), c("Date", "Close", "MA_Short", "MA_Long", "RSI", "MACD", "MACD_Signal", "Signal")]
    validate(need(nrow(ev) > 0, "No Buy or Sell signals for these settings in the selected range."))
    ev$Date <- format(ev$Date, "%Y-%m-%d")
    names(ev) <- c("Date", "Close", "Short MA", "Long MA", "RSI", "MACD", "MACD signal", "Signal")
    ev
  }, digits = 2, striped = TRUE, hover = TRUE)

  output$download_signals <- downloadHandler(
    filename = function() sprintf("%s_%s_signals.csv", symbol_label(), tolower(input$time_frame)),
    content  = function(file) {
      df <- analysis()
      out <- df[, c("Date", "Open", "High", "Low", "Close", "Volume", "MA_Short", "MA_Long",
                    "BB_Upper", "BB_Lower", "RSI", "MACD", "MACD_Signal", "MACD_Hist", "Regime", "Signal")]
      write.csv(out, file, row.names = FALSE)
    }
  )

  # ---- Portfolio tab: several stocks side by side ---------------------------
  portfolio_data <- eventReactive(input$load_portfolio, {
    symbols <- unique(toupper(input$portfolio_symbols))
    validate(need(length(symbols) > 0, "Pick at least one stock."))
    results <- list()
    withProgress(message = "Downloading portfolio", value = 0, {
      for (sym in symbols) {
        incProgress(1 / length(symbols), detail = sym)
        d <- tryCatch(fetch_stock_data(sym, input$date_range[1], input$date_range[2], warmup_days = 0),
                      error = function(e) {
                        showNotification(sprintf("Skipped %s: download failed.", sym), type = "warning")
                        NULL
                      })
        if (is.null(d)) next
        d <- d[index(d) >= input$date_range[1] & index(d) <= input$date_range[2], ]
        if (nrow(d) < 2) next
        results[[sym]] <- data.frame(Date = as.Date(index(d)), Symbol = sym,
                                     Close = as.numeric(d$Close), row.names = NULL)
      }
    })
    validate(need(length(results) > 0, "None of the portfolio stocks could be downloaded."))
    results
  }, ignoreNULL = FALSE)

  output$portfolio_chart <- renderPlot({
    res <- portfolio_data()
    all <- do.call(rbind, lapply(res, function(d) { d$Index <- 100 * d$Close / d$Close[1]; d }))
    ggplot(all, aes(Date, Index, colour = Symbol)) +
      geom_hline(yintercept = 100, colour = "grey60", linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      labs(x = NULL, y = "Growth of 100", colour = NULL,
           title = "Portfolio performance (start of range = 100)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold", colour = col_brand))
  }, res = 96)

  output$portfolio_table <- renderTable({
    res <- portfolio_data()
    do.call(rbind, lapply(res, function(d) {
      r  <- diff(log(d$Close))
      dd <- min(d$Close / cummax(d$Close) - 1)
      data.frame(Symbol = d$Symbol[1],
                 `First close` = d$Close[1], `Last close` = tail(d$Close, 1),
                 `Return %` = 100 * (tail(d$Close, 1) / d$Close[1] - 1),
                 `Volatility % (annual)` = 100 * sd(r) * sqrt(252),
                 `Max drawdown %` = 100 * dd, check.names = FALSE)
    }))
  }, digits = 2, striped = TRUE)
}


# =============================================================================
# RUN THE APP
# =============================================================================
shinyApp(ui, server)
