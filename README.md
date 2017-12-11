# Elixir in 20 Minutes

[Elixir](https://elixir-lang.org/) is a functional programming language built on top of [Erlang](http://www.erlang.org/).
Its focus is on building scalable and maintainable software on distributed and fault-tolerant systems.

## History
- Erlang was created in 1986 by Ericsson to drive their telephone switches.
  - OTP, originally an acronym for Open Telecom Platform, is a collection of components that forms the foundation of Erlang's resilience.
- Elixir was created by José Valim as an R&D project of Plataformatec.
  - José was a member of the Rails core team and wanted to build a friendly language like Ruby.
  - The goal was to build a more extensible and developer-friendly language while staying compatible with Erlang.
- Elixir compiles to bytecode for the BEAM (Erlang's virtual machine).
  - For simple programs, Elixir can be used as a scripting language.

### Syntax Comparison

#### Erlang
```erlang
-module(count_to_ten).
-export([count_to_ten/0]).

count_to_ten() -> do_count(0).

do_count(10) -> 10;
do_count(N) -> do_count(N + 1).
```

#### Elixir
```elixir
def CountToTen do
  def count_to_ten() do
    do_count(0)
  end

  defp do_count(10), do: 10
  defp do_count(n) do
    do_count(n + 1)
  end
end
```

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
- [Elixir Docs](https://elixir-lang.org/getting-started/introduction.html)
- Programming Elixir
- Elixir In Action
