# Minimizing Finite State Automatas

## Usage
`min-dfa [input-file] [output-file]`

## Data format
```dfa
// Order of variables matters so follow the convention

states = {
    s,
    a,
    a0,
    a00,
    b,
    b1,
    b11,
    c,
    c0,
    c01,
    c010,
}

input_alphabet = {0,1}

start_state = s

accept_states = {a00, b11, c010}

delta = 
    s, _ -> a,b,c; // epsilon nfa transition from s to a or from s to b or from s to c

    s, 0 -> c0;
```

Format doesn't differentiate NFA from DFA, because it's compatible with both, however the output will always be an minimal DFA in the input format

charset for identifiers:  [alphanumerical-values] + [any character from: "_-/\\!@#$%^&*()+=[]\"'<>?\`."]