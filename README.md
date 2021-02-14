# quantr

R package for fetching financials and stock market data.

## Usage

### Euronext tickers

```r
euronext_tickers("XBRU")

# A tibble: 144 x 12
   name     isin    symbol market    currency    open   high     low   last last_datetime       volume turnover
   <chr>    <chr>   <chr>  <chr>     <chr>      <dbl>  <dbl>   <dbl>  <dbl> <dttm>               <dbl>    <dbl>
 1 AB INBEV BE0974… ABI    Euronext… EUR      5.23e+1  52.9  5.22e+1  52.6  2021-02-12 17:35:00 1.17e6   6.18e7
 2 ABO GRO… BE0974… ABO    Euronext… EUR      3.80e+0   3.8  3.70e+0   3.7  2021-02-12 16:30:00 2.20e1   8.16e1
 3 ACACIA … GB00BY… ACPH   Euronext… EUR      3.11e+0   3.17 3.10e+0   3.15 2021-02-12 17:35:00 1.32e5   4.15e5
 4 ACCENTIS BE0003… ACCB   Euronext… EUR      4.95e-2   0.05 4.95e-2   0.05 2021-02-12 15:52:00 2.62e4   1.31e3
 5 ACKERMA… BE0003… ACKB   Euronext… EUR      1.30e+2 130    1.28e+2 129.   2021-02-12 17:35:00 1.83e4   2.36e6
 6 ADVICEN… FR0013… ADVIC  Euronext… EUR      1.27e+1  13.8  1.26e+1  13.7  2021-02-12 17:38:00 9.39e4   1.22e6
 7 AEDIFICA BE0003… AED    Euronext… EUR      1.01e+2 102    1.00e+2 100.   2021-02-12 17:35:00 3.09e4   3.10e6
 8 AGEAS    BE0974… AGS    Euronext… EUR      4.56e+1  45.7  4.50e+1  45.4  2021-02-12 17:35:00 2.18e5   9.88e6
 9 AGFA-GE… BE0003… AGFB   Euronext… EUR      3.82e+0   3.82 3.78e+0   3.81 2021-02-12 17:35:00 5.73e4   2.18e5
10 AHOLD D… NL0011… AD     Euronext… EUR      2.31e+1  23.6  2.31e+1  23.5  2021-02-12 17:37:00 4.38e6   1.03e8
# … with 134 more rows
```

### Xetra tickers

```r
xetra_tickers()

# A tibble: 3,073 x 9
   name                      isin        symbol mic   mic_primary mic_reporting currency instrument_type market
   <chr>                     <chr>       <chr>  <chr> <chr>       <chr>         <chr>    <chr>           <chr> 
 1 FACC AG INH.AKT.          AT00000FAC… 1FC    XETR  XWBO        XETB          EUR      CS              Xetra 
 2 RAIFFEISEN BK INTL INH.   AT00006063… RAW    XETR  XWBO        XETB          EUR      CS              Xetra 
 3 PORR AG                   AT00006096… ABS2   XETR  XWBO        XETB          EUR      CS              Xetra 
 4 LENZING AG                AT00006445… LEN    XETR  XWBO        XETB          EUR      CS              Xetra 
 5 ERSTE GROUP BNK INH. O.N. AT00006520… EBO    XETR  XWBO        XETB          EUR      CS              Xetra 
 6 S IMMO AG                 AT00006522… T1L    XETR  XWBO        XETB          EUR      CS              Xetra 
 7 TELEKOM AUSTRIA AG        AT00007200… TA1    XETR  XWBO        XETB          EUR      CS              Xetra 
 8 ANDRITZ AG                AT00007300… AZ2    XETR  XWBO        XETB          EUR      CS              Xetra 
 9 OMV AG                    AT00007430… OMV    XETR  XWBO        XETB          EUR      CS              Xetra 
10 VERBUND AG       INH. A   AT00007464… OEWA   XETR  XWBO        XETB          EUR      CS              Xetra 
# … with 3,063 more rows
```
