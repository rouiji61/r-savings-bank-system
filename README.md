Growth Bank – Simple Savings Banking System
-
A console-based savings banking system developed using R Programming and SQLite as part of our college coursework. The project simulates basic banking operations, allowing users to manage savings accounts, monitor their savings goals, and earn interest based on a three-day no-withdrawal condition.

Features
- 
Create Account – Register a savings account using a full name, password, initial deposit, and preferred annual interest rate.
Automatic Account Number Generation – Automatically assign a unique account number starting from 1001.
User Login – Secure account access using an account number and password.
Check Balance – View current savings balance and annual interest rate.
Deposit Money – Add funds to an existing savings account.
Withdraw Money – Withdraw funds while automatically restarting the three-day interest eligibility period.
Customizable Interest Rate – Set an annual interest rate during account registration.
Three-Day No-Withdrawal Interest – Earn interest after maintaining the required three-day period without withdrawals.
Savings Goals – Set and update personal savings targets.
Savings Progress Tracking – Monitor current savings, target amounts, remaining balance, and completion percentage.
Transaction History – View recorded deposits, withdrawals, and credited interest.

Technologies Used
-
R Programming
SQLite
DBI
RSQLite

Database Structure
-
The system uses three SQLite database tables:
-
accounts - Stores account numbers, passwords, customer names, balances, withdrawal dates, and interest rates.

transactions - Records deposits, withdrawals, and interest credits.

savings_goals - Stores individual savings targets associated with user accounts.

Interest Calculation
-
The system uses a simple annual interest calculation:
Interest = Balance × (Annual Interest Rate / 100) × (Eligible Days / 365)

Interest conditions:
-
Users must maintain their balance for three consecutive calendar days without withdrawing.
A withdrawal resets the three-day waiting period.
Deposits do not reset the waiting period.
Interest is credited when the user performs an account operation after becoming eligible.
Interest is calculated using the account's current balance and annual interest rate.

Getting Started
-
Prerequisites
Make sure you have R or RStudio installed on your computer.
Install Required Packages
Run the following commands in your R console:
-
install.packages("DBI")
install.packages("RSQLite")
