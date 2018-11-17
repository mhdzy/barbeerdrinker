# barbeerdrinker extended

rutgers cs 336 databases project

#### Matthew Handzy (mah376), Juan Cruz (jgc112)

## Overview

## Connecting to our (real) SQL Database

#### host

`barbeerdrinker.cwbowizjcrlm.us-west-1.rds.amazonaws.com`

#### username

`juanmatthew`

#### password

`password`

#### dbname

`barbeerdrinker` (lowercase, case-sensitive)

#### port

`3306`


## Design

### Shiny App

#### `home`
#### `bar`
#### `beer`
#### `drinker`
#### `modification`
#### `sql interface`
#### `readme`

### SQL database

#### `bars`

no change from milestone 2

#### `beers`

no change from milestone 2

#### `bills`

TODO

#### `drinkers`

no change from milestone 2

#### `frequents`

no change from milestone 2

#### `likes`

no change from milestone 2

#### `sells`

no change from milestone 2

#### `transactions`

TODO

## Constraints

#### mandatory constraints:

- [ ] TODO

#### custom constraints:

- [x] proper distribution of bars between tri-state area states
- [x] [[juan]]
- [x] not every drinker frequents a bar
- [x] not every drinker likes beer
- [x] not every bar sells all beers
- [x] transaction time must occur between open-close

## Patterns

#### mandatory patterns:

- [ ] TODO

#### custom patterns:

- [x] real bars from NY, NJ, CT
- [x] real popular beers
  - [x] some manfs sell more than one beer
- [x] generated bills with corresponding bars/frequents and beers/likes per drinker in bills/transactions pairs
- [x] generated pseudo-drinkers
- [x] random frequents pairings
- [x] random likes pairings
- [x] random sells pairings
- [x] transactions derived from generated bills file
  - [x] random tip (10%-20%)
  - [x] randomly assigned timestamp/date
