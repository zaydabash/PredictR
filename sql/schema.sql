CREATE TABLE IF NOT EXISTS accounts (
    account_id INTEGER PRIMARY KEY,
    name TEXT,
    signup_date TEXT,
    region TEXT
);

CREATE TABLE IF NOT EXISTS subscriptions (
    subscription_id INTEGER PRIMARY KEY,
    account_id INTEGER,
    plan_tier TEXT,
    mrr REAL,
    status TEXT,
    start_date TEXT,
    end_date TEXT,
    FOREIGN KEY(account_id) REFERENCES accounts(account_id)
);

CREATE TABLE IF NOT EXISTS events (
    event_id INTEGER PRIMARY KEY,
    account_id INTEGER,
    event_type TEXT,
    timestamp TEXT,
    FOREIGN KEY(account_id) REFERENCES accounts(account_id)
);
