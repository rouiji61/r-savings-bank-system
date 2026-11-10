# ==========================
# LOAD LIBRARIES
# ==========================
library(DBI)
library(RSQLite)

# ==========================
# CONNECT TO DATABASE
# ==========================
con <- dbConnect(SQLite(), "savings_banking_system.db")

# ==========================
# CREATE TABLES
# ==========================
dbExecute(con, "
CREATE TABLE IF NOT EXISTS accounts (
  account_id INTEGER PRIMARY KEY AUTOINCREMENT,
  account_no INTEGER UNIQUE NOT NULL,
  password TEXT NOT NULL,
  name TEXT NOT NULL,
  balance REAL NOT NULL,
  last_withdrawal_date TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS transactions (
  transaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
  account_id INTEGER NOT NULL,
  date TEXT NOT NULL,
  type TEXT,
  amount REAL,
  balance_after REAL,
  FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS savings_goals (
  goal_id INTEGER PRIMARY KEY AUTOINCREMENT,
  account_id INTEGER NOT NULL,
  target_amount REAL NOT NULL CHECK (target_amount > 0),
  created_date TEXT NOT NULL,
  FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);
")

# =========================
# CREATE AN ACCOUNT SECTION 
# =========================
create_account <- function() {
  cat("\n=== CREATE ACCOUNT ===\n")
  
  name <- readline("Full Name: ")
  password <- readline("Password: ")
  initial_balance <- as.numeric(readline("Initial Deposit: "))
  
  if (initial_balance <= 0) {
    cat("Initial deposit must be greater than 0.\n")
    return()
  }
  
  # Generate account number
  last_acc <- dbGetQuery(con, "SELECT MAX(account_no) AS max_no FROM accounts")
  account_no <- ifelse(is.na(last_acc$max_no), 1001, last_acc$max_no + 1)
  
  # Insert account
  dbExecute(con, "
    INSERT INTO accounts (account_no, password, name, balance, last_withdrawal_date)
    VALUES (?, ?, ?, ?, NULL)
  ", params = list(account_no, password, name, initial_balance))
  
  # Get account_id
  account_id <- dbGetQuery(con,
                           "SELECT account_id FROM accounts WHERE account_no = ?",
                           params = list(account_no)
  )$account_id
  
  # Log initial deposit
  dbExecute(con, "
    INSERT INTO transactions (account_id, date, type, amount, balance_after)
    VALUES (?, DATE('now'), 'Deposit', ?, ?)
  ", params = list(account_id, initial_balance, initial_balance))
  
  cat("\n Account created successfully!\n")
  cat("Your Account Number is:", account_no, "\n")
  cat("Please remember your password.\n")
}

# =============
# LOGIN SECTION
# =============
login <- function() {
  cat("\n=== LOGIN ===\n")
  
  acc_no <- as.numeric(readline("Account Number: "))
  password <- readline("Password: ")
  
  user <- dbGetQuery(con,
                     "SELECT * FROM accounts WHERE account_no = ? AND password = ?",
                     params = list(acc_no, password)
  )
  
  if (nrow(user) == 0) {
    cat("Invalid account number or password.\n")
    return(NULL)
  }
  
  cat(" Login successful. Welcome,", user$name, "!\n")
  return(user)
}
while (TRUE) {
  cat("\n=== GROWTH BANK ===\n")
  cat("1. Create Account\n2. Login\n3. Exit\n")
  
  ch <- readline("Choice: ")
  
  if (ch == "1") {
    create_account()
  } else if (ch == "2") {
    user <- login()
    if (!is.null(user)) user_menu(user) #user_menu(for future bank options)
  } else if (ch == "3") {
    break
  } else {
    cat("Invalid choice.\n")
  }
}

dbDisconnect(con)