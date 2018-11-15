bills <- read_csv("~/bills.csv")

# reduce bills to single transaction_id (PK), and include name/bar of tsn
# compute subtotal
tsns <- bills %>% 
  group_by(name, bar, transaction_id) %>% 
  summarise(subtotal = sum(quantity*price)) %>% 
  ungroup()

# generate a random tip between 10-20% of the subtotal
tip <- tbl_df(runif(nrow(tsns), 10, 20))/100

# add tip column and calculate total using 7% sales tax
tsns <- tsns %>% 
  mutate(tip = tip$value) %>% 
  mutate(total = (subtotal + subtotal*tip) + (subtotal + subtotal*tip)*0.07)

tsns$subtotal <- round(tsns$subtotal, 2)
tsns$tip <- round(tsns$tip, 2)
tsns$total <- round(tsns$total, 2)

write_csv(tsns, "~/tsns.csv")