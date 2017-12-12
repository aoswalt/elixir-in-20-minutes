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
- [hexdocs Elixir](https://hexdocs.pm/elixir/)
- [Programming Elixir](https://pragprog.com/book/elixir13/programming-elixir-1-3)
- [Elixir In Action](https://www.manning.com/books/elixir-in-action)
