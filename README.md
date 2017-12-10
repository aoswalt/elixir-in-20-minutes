# Elixir in 20 Minutes
[Elixir](https://elixir-lang.org/getting-started/introduction.html)

compiled & scripts

## History
- on top of Erlang
- built by Jose
- to be like Ruby

## Types
- atom (symbol) (alias)
- boolean
- integer
- float
- string (binary) (charlist) (sigil)
- tuple
- list (linked) (iodata)
- keyword list
- map
- struct
- function (named & anonymous) (arity)
  - capturing
  - default arguments

is_type

## Operators
- arithmetic (+, -, *, /, div, rem)
- list manipulation (++, --)
- string concatenation (<>)
- boolean (or, and, not) (require boolean, short circuit)
  - (||, &&, !) (any type, all but false and nil are true)
- comparison (==, !=, ===, !==, >=, <=, <, >)
- data type ordering (number < atom < reference < function < port < pid < tuple < map < list < bitstring)
- pin (assignment)
- pipe

## Code Organization
### Functions
### Modules (alias, require, import, use) (attributes)
### Applications

## Pattern Matching
railway oriented / happy-path programming - with

## Control Flow
### if/unless
### cond (if/unless macros to cond)
### case
### do/end vs keyword list
### with

## Recursion
### for is mutability
#### comprehensions
### reduce & map algorithms
### Enumerable (eager Enum vs lazy Stream)

## Protocols (polymorphism)

## Behaviours (apis)

## Error Handling
### error
### throw
### exit
### let it crash

## Tooling
### ExUnit (doctest)
### Mix
### IEx
### types (docs and dialyzer)

## processes
### lightweight (!= threads)
### mailboxes & messaging
### linking
### state via receive recursion
### tasks & agents

## OTP
### GenServer
### Supervisor
### supervision trees (let it crash)
### ETS
### Mnesia
### Applications
### Umbrella Applications

## Macros


## Resources
- Programming Elixir
- Elixir In Action
