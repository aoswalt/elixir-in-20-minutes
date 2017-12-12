# Elixir in 20 Minutes

[Elixir](https://elixir-lang.org/) is a functional programming language built on top of [Erlang](http://www.erlang.org/).
Its focus is on building scalable and maintainable software on distributed and fault-tolerant systems.

#### Web Server Comparison
[![Web Server Comparsion](./images/web-server-comparison.jpg "Web Server Comparison")](https://www.manning.com/books/elixir-in-action)

## History
- Erlang was created in 1986 by Ericsson to drive their telephone switches.
  - OTP, originally an acronym for Open Telecom Platform, is a collection of components that comprise the foundation of Erlang's resilience.
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
- nil
- atom (symbol) `:atom`
  - alias `Atom == :"Elixir.Atom"`
- boolean `true == :true`
- integer `1000000 == 1_000_000`
- float `0.01 == 1.0e-2`
- string (binary) `"hi" == <<104, 105>>`
  - charlist `'hi' == [104, 105]`
  - sigil `"foo" =~ ~r/foo|bar/` `~r"foo|bar"i == sigil_r(<<"foo|bar">>, 'i')`
  - heredoc
  ```elixir
  """
  this is
  a heredoc string
  """
  ```
- tuple `{:ok, {:sent, "data"}}`
- list `[1, "b", :c, true]`
  - linked `[1 | [2 | [3 | []]]]`
  - io list `["Hello, ", [87, 111, 114, 108, 100]] == "Hello, World"`
- keyword list `[{:a, 1}, {:a, 0}, {:b, 2}] == [a: 1, a: 0, b: 2]`
  - accessing `kw_list[:a] == 1`
- map `%{"a" => 1, :b => 2, 3 => "c"}` `%{a: 1, b: 2}`
  - accessing `map[:a] == map.a`
- struct `%City{name: "Nashville"} == %{__struct__: City, name: "Nashville"}`
- function
  ```elixir
  def add(a, b) do
    a + b
  end
  ```
  - keyword list format
  ```elixir
  def add(a, b), do: a + b
  ```
  - anonymous
  ```elixir
  add = fn (a, b) -> a + b end
  add.(1, 2)
  ```
  - arity
  ```elixir
  def add(a, b), do: a + b          # add/2
  def add(a, b, c), do: a + b + c   # add/3
  ```
  - capturing
  ```elixir
  def add(a, b), do: a + b
  fun = &add/2
  ```
  - default arguments
  ```elixir
  def hello(name \\ "world") do
    "hello, " <> name
  end
  ```

Types can be checked with the `is_type` functions, such as `is_atom`, `is_binary`, `is_map`, etc.

## Operators
- arithmetic (`+`, `-`, `*`, `/`, `div`, `rem`)
  - `/` always produces a float result
  - `div` and `rem` are integer division and remainder operators
- list manipulation (`++`, `--`)
  ```elixir
  [1, 2, 3] ++ [4, 5, 6] == [1, 2, 3, 4, 5, 6]
  [1, 2, 3] -- [2] == [1, 3]
  ```
- string concatenation (`<>`)
  ```elixir
  "foo" <> "bar" == "foobar"
  ```
- strict boolean (`or`, `and`, `not`)
  - require boolean as first operand
- lax boolean (`||`, `&&`, `!`)
  - accepts any type
  - everything except false and nil are true
- comparison (`==`, `!=`, `===`, `!==`, `>=`, `<=`, `<`, `>`)
  ```elixir
  1 == 1.0   # true
  1 === 1.0  # false
  ```
  - data type ordering
    ```elixir
    number < atom < reference < function < port < pid < tuple < map < list < bitstring
    ```
- match (`=`)
  ```elixir
  a = "a"   # "a"
  "a" = a   # "a"
  "b" = a   # MatchError
  ```
- pin (`^`)
  ```elixir
  a = "a"   # "a"
  ^a = "b"  # MatchError
  ```
- pipe (`|>`)
  ```elixir
  String.replace(String.upcase("hello world"), " ", "_")

  "hello world"
  |> String.upcase
  |> String.replace(" ", "_")
  ```

## Code Organization

### Functions

Except for a small set of core functions that operate at the system level, such as `Calendar.date/0`,
all functions in Elixir transform input into output.

### Modules (alias, require, import, use) (attributes)

Modules serve as a mechanism to group related functions and provide a namespace.

At compile time, modules may be augmented by the `alias`, `require`, and `import` directives and the `use` macro.
- `alias` allows for creating more friendly names for modules.
Calling without the `:as` option uses the last part of the module name.
```elixir
alias Math.Coordinate, as: Coord
alias Math.Coordinate
```
- `require` allows using macros defined in a module.
```elixir
require Integer
Integer.is_odd(5)
```
- `import` provides access to fuctions or macros without using the full name.
```elixir
import Enum, only: [map: 2]
```
- `use` requires a module and invokes its `__using__/1` macro to inject code into the calling module.
```elixir
use Feature, option: :value
```

Multiple modules may be referenced by the same directive by using braces.
```elixir
alias MyApp.{Foo, Bar, Baz}
```

Module attributes, defined with `@` such as `@limit 10`, are commonly used to defined module scoped constants.

Defining a struct inside a module creates a tagged map that allows for
compile time guarantees, default values, and a common data element to build functions around.

### Applications

An application is a complete component providing a specific set of functionality that operates as a unit and
may be re-used in other systems.
Applications form the building blocks of a full Elixir system.

An example web server may include a set of distinct applications for its separate concerns:
- web api
- database access
- encapsulated library of business logic
- event sourcing handlers

## Pattern Matching

Pattern matching forms a fundamental part of control flow in elixir.

A function may be defined with multiple clauses.
When called, each clauses will be tested in the order defined until a match is found.
If no match is found, an error is raised.

```elixir
defmodule Math do
  def zero?(0), do: true
  def zero?(n) when is_integer(n), do: false
end

Math.zero?(0) == true
Math.zero?(1) == false
Math.zero?(0.5)  # FunctionClauseError
```

Pattern matching enables a programming style known as railway-oriented or happy-path programming.
Functions with a more likely possiblity of failure typically return `:ok`/`:error` tuples
such as `{:ok, data}` and `{:error, error}`.

This allows for functions to be piped together.
The successful operations continue being processed while any errors are rerouted to be handled gracefully.

A common practice is to use `with` to handle the branching paths.

```elixir
with {:ok, response} <- Api.fetch(@url),
     mapped_data = Enum.map(response, &format/1),
     {:ok, processed} <- Lib.process(mapped_data) do
  {:ok, processed}
else
  {:error, {:network_error, fetch_details}} ->
    {:error, format_error_details(details)},
  {:error, {:processing_error, processing_details}} ->
    {:error, processing_details},
  error ->
    {:error, error}
end
```

## Control Flow

While pattern matching is the most common method of control flow, other structures are used to varying degrees.

- `case` compares a value against patterns until a matching one is found.
```elixir
case {2, 1} do
  {0, 0} -> "At the origin"
  {0, y} -> "On the X axis at #{y}"
  {x, 0} -> "On the Y axis at #{x}"
  {_, _} -> "Not axis aligned"
  _ -> "Not a valid point"
end
```

- `cond` tests multiple conditions until one evaluates to true (any value besides `nil` and `false`).
```elixir
cond do
  2 + 2 == 5 ->
    "This is never true"
  2 * 2 == 3 ->
    "Nor this"
  true ->
    "This is always true (equivalent to else)"
end
```

- `if` and `unless` are commonly not needed but can be useful if checking for a single condition.
```elixir
if true do
  "Do the thing"
end
# returns nil
```

```elixir
unless true do
  "Unless matches if false"
else
  "So this is the result"
end
```

Insead of special constructs, `if` and `unless` are implemented as macros around `cond`.

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
- [hexdocs Elixir](https://hexdocs.pm/elixir/)
- [Learn Elixir in Y minutes](https://learnxinyminutes.com/docs/elixir/)
- [Programming Elixir](https://pragprog.com/book/elixir13/programming-elixir-1-3)
- [Elixir In Action](https://www.manning.com/books/elixir-in-action)
