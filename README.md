# [WIP] Minimizing Finite State Automatas

## Usage
`min-dfa [input-file] [output-file]`

## Data format
Following: [https://web.cs.ucdavis.edu/~doty/automata/](https://web.cs.ucdavis.edu/~doty/automata/)

with exception to delta relation:
```dfa
delta = 
    fromState, symbol -> toState; // Basic transition
    fromState, _ -> toState; // Epsilon transition
    fromState, symbol -> toState1, toState2 // Non deterministic transition
    fromState, _ -> toState1, toState2 // Non deterministic epsilon transition
```

## 