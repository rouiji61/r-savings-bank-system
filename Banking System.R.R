
# ==========================================
# GROWTH BANK - SAVINGS BANKING SYSTEM
# R PROGRAMMING + SQLITE
# ==========================================

library(DBI)
library(RSQLite)

DB_FILE <- "savings_banking_system.db"

# ==========================================
# DATABASE CONNECTION
# ==========================================

create_connection <- function() {
  dbConnect(SQLite(), DB_FILE)
}

# ==========================================
# INITIALIZE DATABASE
# ==========================================

initialize_database <- function() {

  con <- create_connection()

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS accounts (
      account_id INTEGER PRIMARY KEY AUTOINCREMENT,
      account_no INTEGER UNIQUE NOT NULL,
      password TEXT NOT NULL,
      name TEXT NOT NULL,
      balance REAL NOT NULL,
      last_withdrawal_date TEXT,
      created_date TEXT,
      interest_rate REAL DEFAULT 0,
      last_interest_date TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      transaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
      account_id INTEGER NOT NULL,
      date TEXT NOT NULL,
      type TEXT,
      amount REAL,
      balance_after REAL,
      FOREIGN KEY (account_id)
      REFERENCES accounts(account_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS savings_goals (
      goal_id INTEGER PRIMARY KEY AUTOINCREMENT,
      account_id INTEGER NOT NULL,
      target_amount REAL NOT NULL
        CHECK (target_amount > 0),
      created_date TEXT NOT NULL,
      FOREIGN KEY (account_id)
      REFERENCES accounts(account_id)
    )
  ")

  # Add columns when opening an older database

  fields <- dbListFields(con, "accounts")

  migrations <- list(
    created_date = "TEXT",
    interest_rate = "REAL DEFAULT 0",
    last_interest_date = "TEXT"
  )

  for (field in names(migrations)) {
    if (!(field %in% fields)) {
      dbExecute(
        con,
        paste(
          "ALTER TABLE accounts ADD COLUMN",
          field,
          migrations[[field]]
        )
      )
    }
  }

  # Initialize dates for existing accounts

  dbExecute(con, "
    UPDATE accounts
    SET created_date = DATE('now')
    WHERE created_date IS NULL
  ")

  dbExecute(con, "
    UPDATE accounts
    SET last_interest_date = created_date
    WHERE last_interest_date IS NULL
  ")

  dbDisconnect(con)

  cat("Database initialized successfully!\n")
}

initialize_database()

# ==========================================
# HELPER FUNCTIONS
# ==========================================

get_date <- function() {
  format(Sys.Date(), "%Y-%m-%d")
}

get_account <- function(account_id) {

  con <- create_connection()

  user <- dbGetQuery(
    con,
    "SELECT * FROM accounts WHERE account_id = ?",
    params = list(account_id)
  )

  dbDisconnect(con)

  if (nrow(user) == 0) return(NULL)

  user
}

record_transaction <- function(
  account_id,
  type,
  amount,
  balance
) {

  con <- create_connection()

  dbExecute(con, "
    INSERT INTO transactions
    (account_id, date, type, amount, balance_after)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    account_id,
    get_date(),
    type,
    amount,
    balance
  ))

  dbDisconnect(con)
}

# ==========================================
# INTEREST CALCULATION
# THREE DAYS WITHOUT WITHDRAWAL
# ==========================================

apply_interest <- function(account_id) {

  user <- get_account(account_id)

  if (is.null(user)) return()

  balance <- user$balance[1]
  rate <- user$interest_rate[1]

  if (is.na(rate) || rate <= 0) return()

  start_date <- as.Date(
    user$last_interest_date[1]
  )

  if (!is.na(user$last_withdrawal_date[1])) {

    withdrawal_date <- as.Date(
      user$last_withdrawal_date[1]
    )

    start_date <- max(
      start_date,
      withdrawal_date
    )
  }

  elapsed_days <- as.integer(
    Sys.Date() - start_date
  )

  # Require three complete days

  eligible_blocks <- floor(elapsed_days / 3)

  if (eligible_blocks < 1) return()

  eligible_days <- eligible_blocks * 3

  # Simple interest calculation

  interest <- balance *
    (rate / 100) *
    (eligible_days / 365)

  interest <- round(interest, 2)

  new_balance <- round(
    balance + interest,
    2
  )

  new_interest_date <- start_date +
    (eligible_days)

  con <- create_connection()

  dbExecute(con, "
    UPDATE accounts
    SET balance = ?,
        last_interest_date = ?
    WHERE account_id = ?
  ", params = list(
    new_balance,
    as.character(new_interest_date),
    account_id
  ))

  dbDisconnect(con)

  if (interest > 0) {

    record_transaction(
      account_id,
      "Interest",
      interest,
      new_balance
    )

    cat("\n================================\n")
    cat("       INTEREST EARNED\n")
    cat("================================\n")
    cat("Eligible days:", eligible_days, "\n")
    cat("Interest rate:", rate, "% annually\n")
    cat("Interest credited: PHP", interest, "\n")
    cat("New balance: PHP", new_balance, "\n")
  }
}

# ==========================================
# CREATE ACCOUNT
# ==========================================

create_account <- function() {

  cat("\n========== CREATE ACCOUNT ==========\n")

  name <- trimws(readline("Full Name: "))
  password <- readline("Password: ")

  initial_balance <- suppressWarnings(
    as.numeric(readline("Initial Deposit: "))
  )

  rate <- suppressWarnings(
    as.numeric(readline("Annual Interest Rate (%): "))
  )

  if (
    name == "" ||
    password == "" ||
    is.na(initial_balance) ||
    is.na(rate) ||
    initial_balance <= 0 ||
    rate < 0 ||
    rate > 100
  ) {
    cat("Invalid input. Please try again.\n")
    return()
  }

  con <- create_connection()

  last_acc <- dbGetQuery(
    con,
    "SELECT MAX(account_no) AS max_no FROM accounts"
  )

  account_no <- if (
    is.na(last_acc$max_no[1])
  ) {
    1001
  } else {
    last_acc$max_no[1] + 1
  }

  today <- get_date()

  dbExecute(con, "
    INSERT INTO accounts (
      account_no,
      password,
      name,
      balance,
      last_withdrawal_date,
      created_date,
      interest_rate,
      last_interest_date
    )
    VALUES (?, ?, ?, ?, NULL, ?, ?, ?)
  ", params = list(
    account_no,
    password,
    name,
    initial_balance,
    today,
    rate,
    today
  ))

  account_id <- dbGetQuery(
    con,
    "SELECT account_id FROM accounts WHERE account_no = ?",
    params = list(account_no)
  )$account_id[1]

  dbDisconnect(con)

  record_transaction(
    account_id,
    "Deposit",
    initial_balance,
    initial_balance
  )

  cat("\nAccount created successfully!\n")
  cat("Bank: GROWTH BANK\n")
  cat("Account Number:", account_no, "\n")
  cat("Annual Interest Rate:", rate, "%\n")
  cat("Initial Balance: PHP", initial_balance, "\n")
}

# ==========================================
# LOGIN
# ==========================================

login <- function() {

  cat("\n========== LOGIN ==========\n")

  acc_no <- suppressWarnings(
    as.numeric(readline("Account Number: "))
  )

  password <- readline("Password: ")

  if (is.na(acc_no) || password == "") {
    cat("Invalid input.\n")
    return(NULL)
  }

  con <- create_connection()

  user <- dbGetQuery(con, "
    SELECT * FROM accounts
    WHERE account_no = ? AND password = ?
  ", params = list(acc_no, password))

  dbDisconnect(con)

  if (nrow(user) == 0) {
    cat("Invalid account number or password.\n")
    return(NULL)
  }

  cat("\nLogin successful!")
  cat("\nWelcome,", user$name[1], "!\n")

  user
}

# ==========================================
# CHECK BALANCE
# ==========================================

check_balance <- function(account_id) {

  apply_interest(account_id)

  user <- get_account(account_id)

  cat("\n========== ACCOUNT BALANCE ==========\n")
  cat("Account Holder:", user$name[1], "\n")
  cat("Account Number:", user$account_no[1], "\n")
  cat("Current Balance: PHP", user$balance[1], "\n")
  cat("Annual Interest Rate:", user$interest_rate[1], "%\n")
}

# ==========================================
# DEPOSIT
# ==========================================

deposit_money <- function(account_id) {

  amount <- suppressWarnings(
    as.numeric(readline("Deposit Amount: "))
  )

  if (is.na(amount) || amount <= 0) {
    cat("Invalid deposit amount.\n")
    return()
  }

  apply_interest(account_id)

  user <- get_account(account_id)

  new_balance <- round(
    user$balance[1] + amount,
    2
  )

  con <- create_connection()

  dbExecute(con, "
    UPDATE accounts
    SET balance = ?
    WHERE account_id = ?
  ", params = list(new_balance, account_id))

  dbDisconnect(con)

  record_transaction(
    account_id,
    "Deposit",
    amount,
    new_balance
  )

  cat("\nDeposit successful!\n")
  cat("New Balance: PHP", new_balance, "\n")
}

# ==========================================
# WITHDRAWAL
# ==========================================

withdraw_money <- function(account_id) {

  apply_interest(account_id)

  user <- get_account(account_id)

  amount <- suppressWarnings(
    as.numeric(readline("Withdrawal Amount: "))
  )

  if (is.na(amount) || amount <= 0) {
    cat("Invalid withdrawal amount.\n")
    return()
  }

  if (amount > user$balance[1]) {
    cat("Insufficient balance.\n")
    return()
  }

  new_balance <- round(
    user$balance[1] - amount,
    2
  )

  con <- create_connection()

  dbExecute(con, "
    UPDATE accounts
    SET balance = ?,
        last_withdrawal_date = ?,
        last_interest_date = ?
    WHERE account_id = ?
  ", params = list(
    new_balance,
    get_date(),
    get_date(),
    account_id
  ))

  dbDisconnect(con)

  record_transaction(
    account_id,
    "Withdrawal",
    amount,
    new_balance
  )

  cat("\nWithdrawal successful!\n")
  cat("New Balance: PHP", new_balance, "\n")
  cat("Your three-day interest period has restarted.\n")
}

# ==========================================
# SAVINGS GOAL
# ==========================================

set_savings_goal <- function(account_id) {

  target <- suppressWarnings(
    as.numeric(readline("Enter Savings Goal: "))
  )

  if (is.na(target) || target <= 0) {
    cat("Invalid savings goal.\n")
    return()
  }

  con <- create_connection()

  existing <- dbGetQuery(con, "
    SELECT goal_id FROM savings_goals
    WHERE account_id = ?
  ", params = list(account_id))

  if (nrow(existing) > 0) {

    dbExecute(con, "
      UPDATE savings_goals
      SET target_amount = ?,
          created_date = ?
      WHERE account_id = ?
    ", params = list(
      target,
      get_date(),
      account_id
    ))

  } else {

    dbExecute(con, "
      INSERT INTO savings_goals
      (account_id, target_amount, created_date)
      VALUES (?, ?, ?)
    ", params = list(
      account_id,
      target,
      get_date()
    ))
  }

  dbDisconnect(con)

  cat("\nSavings goal saved successfully!\n")
}

# ==========================================
# VIEW SAVINGS GOAL
# ==========================================

view_savings_goal <- function(account_id) {

  user <- get_account(account_id)

  con <- create_connection()

  goal <- dbGetQuery(con, "
    SELECT * FROM savings_goals
    WHERE account_id = ?
  ", params = list(account_id))

  dbDisconnect(con)

  cat("\n========== SAVINGS GOAL ==========\n")

  if (nrow(goal) == 0) {
    cat("No savings goal has been set.\n")
    return()
  }

  balance <- user$balance[1]
  target <- goal$target_amount[1]

  progress <- min(
    (balance / target) * 100,
    100
  )

  remaining <- max(target - balance, 0)

  cat("Savings Target: PHP", target, "\n")
  cat("Current Savings: PHP", balance, "\n")
  cat("Remaining Amount: PHP", remaining, "\n")
  cat("Progress:", round(progress, 2), "%\n")

  if (balance >= target) {
    cat("Congratulations! Savings goal achieved!\n")
  }
}

# ==========================================
# TRANSACTION HISTORY
# ==========================================

view_transactions <- function(account_id) {

  con <- create_connection()

  transactions <- dbGetQuery(con, "
    SELECT date, type, amount, balance_after
    FROM transactions
    WHERE account_id = ?
    ORDER BY transaction_id DESC
  ", params = list(account_id))

  dbDisconnect(con)

  cat("\n========== TRANSACTION HISTORY ==========\n")

  if (nrow(transactions) == 0) {
    cat("No transactions found.\n")
    return()
  }

  print(transactions)
}

# ==========================================
# USER MENU
# ==========================================

user_menu <- function(user) {

  account_id <- user$account_id[1]

  repeat {

    cat("\n====================================\n")
    cat("             GROWTH BANK\n")
    cat("====================================\n")
    cat("1. Check Balance\n")
    cat("2. Deposit Money\n")
    cat("3. Withdraw Money\n")
    cat("4. Set Savings Goal\n")
    cat("5. View Savings Goal\n")
    cat("6. View Transaction History\n")
    cat("7. Logout\n")
    cat("====================================\n")

    choice <- readline("Enter Choice: ")

    if (choice == "1") {

      check_balance(account_id)

    } else if (choice == "2") {

      deposit_money(account_id)

    } else if (choice == "3") {

      withdraw_money(account_id)

    } else if (choice == "4") {

      set_savings_goal(account_id)

    } else if (choice == "5") {

      view_savings_goal(account_id)

    } else if (choice == "6") {

      view_transactions(account_id)

    } else if (choice == "7") {

      cat("\nLogged out successfully.\n")
      break

    } else {

      cat("Invalid choice. Please try again.\n")
    }
  }
}

# ==========================================
# MAIN MENU
# ==========================================

repeat {

  cat("\n====================================\n")
  cat("             GROWTH BANK\n")
  cat("       SAVINGS BANKING SYSTEM\n")
  cat("====================================\n")
  cat("1. Create Account\n")
  cat("2. Login\n")
  cat("3. Exit\n")
  cat("====================================\n")

  choice <- readline("Enter Choice: ")

  if (choice == "1") {

    create_account()

  } else if (choice == "2") {

    user <- login()

    if (!is.null(user)) {
      user_menu(user)
    }

  } else if (choice == "3") {

    cat("\nThank you for using Growth Bank!\n")
    break

  } else {

    cat("Invalid choice. Please try again.\n")
  }
}
