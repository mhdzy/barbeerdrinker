# barbeerdrinker

###### minimum requirements (front end / queries)

### bar page:
`given a bar, show`
- [x] top drinkers who are largest spenders
- [x] beers which are most popular
- [x] manufacturers who sell the most beers
- [x] demonstrate time distribution of sales
  - busiest periods of the day and of the week

### beer page:
`given a beer, show`
- [x] top bars where this beer sells the most
- [x] drinkers who are the biggest consumers of this beer
- [x] time distribution of when this beer sells the most

### drinker page:
`given a drinker, show`
  - [x] all transactions ordered by time and grouped by different bars
  - [x] bar graphs of beers he/she orders the most
  - [x] bar graph of his/her spending in different bars
    - different dates/weeks/months

### modification page:
- [x] allow end user to modify every table in your databases
- [x] have a way to add a new transaction for a given date
- [x] Have MODIFICATION page, with one box for each table. If update is not accepted – provide the feedback message “violates foreign key”, etc
- [x] the integrity constraints should be implemented
- [x] foreign keys for each of the three tables – frequents, likes and sells. drinker, bar and beer should be present in tables drinker, bar, beer *before* they can participate in tables frequent, likes and sells.
- [x] key constraint for sells table – on bar, beer (bar, beer -> price)
  #### assertions
- [x] all three patterns (1-3) from above have to be enforced as assertions. this means if an update (insert, update) violates the assertion it *should not be allowed* nd proper warning/explanation has to be displayed – “not accepted due to violation of assertion X”

### sql query interface:
- [x] provide a box where we can type in sql query and get them evaluated on your database (optional / extra credit)

#### final submission should contain
- [x] ER diagram and Relational DB scheme
- [x] URL for web application
- [x] presentation (up to 10-15 slides) / markdown files
- [x] source code
