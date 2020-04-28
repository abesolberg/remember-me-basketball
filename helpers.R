# Helpers

## Things to do:
  # Chaos Mode
  # Fix Players Getting Cut off
  # Toggle Front-Back
  # Background
  # Strip at Bottom (pending background)

library(tidyverse)

# Read Data ----

data <- read_rds('data.RDS')
types <- read_rds('type_hier.RDS')

## Scoreboard: Created by https://codepen.io/Cassin8ter/pen/XzzMpr



css <- '
.count {
display:flex;
flex-direction: row;
font-size: 1.5rem;
padding-top: .5rem;
align-items:center;
}


#BONUS {
padding-right: 1rem;
display: flex;
flex-direction: column;
align-items: center;
}

#POSS-R {
padding-right: 1rem;
display: flex;
flex-direction: column;
align-items: center;
}

#POSS-F {
padding-right: 1rem;
display: flex;
flex-direction: column;
align-items: center;
}


#BONUS:after {
content: "●●";
color: yellow;
}

#POSS-R:after {
content: "●";
color:yellow;
}

#POSS-F:after {
content: "●";
color:#EEEEEE;
}
'


