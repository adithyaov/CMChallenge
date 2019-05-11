# ReadMe :-)

## Installation
`cabal new-install CMChallenge`

## Usage
`CMChallenge srcFilePath destFilePath Option`
where `Option = FINAL | STEPS` 

`FINAL` will result in the final amounts paid by
the districts.

`STEPS` will result in amount paid/recieved in all
the iterations. (+) is paid and (-) is recieved.

## Input JSON format
Also attached is `schemaExample/test.json` which will give a brief
idea of the input format. The format is ugly but is 
convinient for me to work with. Fortunately the problem
statement gives me the liberty to choose an input schema.

## Output JSON format
There are 2 output files, namely, `schemaExample/steps.json`
and `schemaExample/final.json`
which help to provide an idea of the output schema. `steps.json`
provides a list of allocation (+) and (-) and is ordered with the
head containing the most recent allocation.

## Theory/Idea
Basically the problem is to distribute the available
resources while following the *law of proportionality*.

We have 3 laws as described in the problem statement,
1. Division of available resources.
2. Division of resources provided among districts.
3. Division of resources because of capacity constraints.

The basic idea of the solution is to iterate a certain process
(described below) while following the laws of proportionality
(analogous to fixed point calculations).

It is easier to understand the solution if you think of the
following terms in a specific way.

1. pledge: Think of this as something that the district owes.
   If the bill is completed funded then the pledge can become 0,
   ie. The district does not owe anything.
2. cap: This is the limit of what a district can give to a specific
   category/ministry.
3. available: This is the maximum available that a district can
   give.

At every iteration we create a new problem to work on.
Every iteration has 2 stages,

First stage is where a district gives the maximum
amount possible irrespective of any other district
following Law 1 and Law 3 of proportionality.
The pledge, available and the capacity should be modified
accordingly. For eg. if the district gives 30 for bill 1
of category A. At this point assume that the capacity
and the available are respectively 50 and 60, the capacity for
category A should be modified to 20 and the plegde
for bill 1 under category A should be modified to 30.

Second stage is where the extra portions recieved for
each bills are returned following the Law 2 of proportionality.
The pledge, available and the capacity should be modified
accordingly here as well. If there is extra portion for a specific
bill 1 under category A then the pledge for bill 1 under capacity
A should be made 0. This is because a district does not need to
owe anything towards that bill anymore. The available and capacity 
for category A will increase. For eg, if there is 20 extra for
bill 1 of category A then for any district d, a fraction of 20
according to what d gave in Stage 1 will be returned back and
hence available for d and capacity for category A will increase
by that fraction.

At this point, after the iteration, we have a new problem with
new pledges, new capacities and new available funds for each
district. We continue iterating till we reach an equilibrium
which will result in the final allocation.

Due to integer division, the resulting allocation might be 1 dollar 
more than the required. There might also be an inconsistancy
of 1 or 2 dollars if the numbers given are such. The inconsistancy
can be eliminated given a few more passes but that is an overkill
for such a small problem and is not implemented.






