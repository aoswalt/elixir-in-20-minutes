# Elixir in 20 Minutes

[Full Reference](./reference.md)

[Elixir](https://elixir-lang.org/) is a functional programming language built on top of [Erlang](http://www.erlang.org/).
By compiling to the BEAM (Erlang's virtual machine), Elixir can leverage all of the power of the OTP system while having a
more user-friendly syntax that is scalable and maintainable.
OTP, originally an acronym for Open Telecom Platform, is a collection of components that comprise the foundation of Erlang's resilience.

#### Web Server Comparison
[![Web Server Comparsion](./images/web-server-comparison.jpg "Web Server Comparison")](https://www.manning.com/books/elixir-in-action)

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
- atom (symbol)
  ```elixir
  :atom
  ```
- tuple
  ```elixir
  {:ok, "response"}
  {:ok, {:sent, "data"}}
  ```
- list
  ```elixir
  [1, "b", :c, true]
  ```
- keyword list
  ```elixir
  [{:a, 1}, {:a, 0}, {:b, 2}] == [a: 1, a: 0, b: 2]

  # access with the atom, retrieving the first result
  kw_list[:a] == 1
  ```
- map
  ```elixir
  map = %{"a" => 1, :b => 2, 3 => "c"}
  map["a"] == 1

  atom_map = %{a: 1, b: 2}
  atom_map.a == 1
  ```
- struct
  ```elixir
  %City{name: "Nashville"} == %{__struct__: City, name: "Nashville"}
  ```
- function
  ```elixir
  def add(a, b) do
    a + b
  end

  # equivalent with keyword list instead of do/end block
  def add(a, b), do: a + b
  ```
  - anonymous
    ```elixir
    add = fn (a, b) -> a + b end
    add.(1, 2)
    ```
  - arity
    ```elixir
    # different arities are different functions
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

## Operators
- list manipulation (`++`, `--`)
  ```elixir
  [1, 2, 3] ++ [4, 5, 6] == [1, 2, 3, 4, 5, 6]
  [1, 2, 3] -- [2] == [1, 3]
  ```
- string concatenation (`<>`)
  ```elixir
  "foo" <> "bar" == "foobar"
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

  # output becomes the first argument of the following
  "hello world"
  |> String.upcase
  |> String.replace(" ", "_")

  # result: "HELLO_WORLD"
  ```

## Code Organization

### Functions

Except for a small set of core functions that operate at the system level, such as `Calendar.date/0`,
all functions in Elixir transform input into output.

### Modules

Modules serve as a mechanism to group related functions and provide a namespace.

Module attributes, defined with `@` such as `@limit 10`, are commonly used to defined module scoped constants.

### Applications

An application is a complete component providing a specific set of functionality that operates as a unit and
may be re-used in other systems.

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

## Control Flow

While pattern matching is the most common method of control flow, other structures are used to varying degrees.

- `case` compares a value against patterns until a matching one is found.
  ```elixir
  point = {2, 1}

  case point do
    {0, 0} -> "At the origin"
    {0, y} -> "On the X axis at #{y}"
    {x, 0} -> "On the Y axis at #{x}"
    {_, _} -> "Not axis aligned"
    _ -> "Not a valid point"
  end

  # returns "Not axis aligned"
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

  unless true do
    "Unless matches if false"
  else
    "So this is the result"
  end
  ```

## Recursion

Functional languages make use of recursion more commonly than object oriented languages due to immutability.
Common `for` loops rely on mutability in incrementing the counter.

Pattern matching allows for simpler recurive functions by separating conditions into separate clauses.
```elixir
def print_repeated(msg, count) when count <= 1 do
  IO.puts msg
end

def print_repeated(msg, count) do
  IO.puts msg
  print_repeated(msg, count - 1)
end

print_repeated("echo", 4)   # prints "echo" 4 times
```

## Protocols

Protocols are the primary method of polymorphism in Elixir.

Data types that implement a protocol define implementations of functions that are applicable across varied
types.  Implementations of a protocol may be defined at the data type definition or separately
(for built-in types, etc.).

```elixir
defprotocol Size do
  @doc "Calculates the size (not the length!) of a data structure"
  def size(data)
end

# BitString, Map, and Tuple are different types that have a size
defimpl Size, for: BitString do
  def size(string), do: byte_size(string)
end

defimpl Size, for: Map do
  def size(map), do: map_size(map)
end

defimpl Size, for: Tuple do
  def size(tuple), do: tuple_size(tuple)
end

Size.size("hello") == 5
Size.size({:ok, "data"}) == 2
Size.size(%{a: 1}) == 1
Size.size([1, 2, 3])  # Protocol.UndefinedError
```

### Enumerable

Data structures in Elixir are enumerable if they implement the `Enumerable` protocol.

These data structures, including lists and maps, work with functions in the `Enum` module and its lazy
counterpart the `Stream` module.
These modules provide methods, such as `map`, `filter`, and `reduce`, for working with collections of data.

- Sum the double value of all even numbers from 1 to 100,000, iterating over the collection for each operation.
  ```elixir
  1..100_000
  |> Enum.filter(&Integer.is_even/1)
  |> Enum.map(&(&1 * 2))
  |> Enum.sum
  ```
- Using the lazy `Stream`, the collection is only iterated over once.
  ```elixir
  1..100_000
  |> Stream.filter(&Integer.is_even/1)
  |> Stream.map(&(&1 * 2))
  |> Enum.sum
  ```

## Behaviours

Similar to interfaces in object oriented languages, behaviours define a public API that must be
implemented for a module to encapsulate varying sets of logic in the same types of modules.

```elixir
defmodule Parser do
  @callback parse(String.t) :: {:ok, term} | {:error, String.t}
  @callback extensions() :: [String.t]

  def parse!(implementation, contents) do
    case implementation.parse(contents) do
      {:ok, data} -> data
      {:error, error} -> raise ArgumentError, "parsing error: #{error}"
    end
  end
end


# JSONParser and YAMLParser are both parsers but different types
defmodule JSONParser do
  @behaviour Parser

  def parse(str), do: # ... parse JSON
  def extensions, do: ["json"]
end

defmodule YAMLParser do
  @behaviour Parser

  def parse(str), do: # ... parse YAML
  def extensions, do: ["yml"]
end
```

## Tooling

Elixir includes a robust set of tooling to aid in development.

### Mix

Akin to Node's NPM and Ruby's rake, Elixir comes with the general purpose build tool Mix.

`mix new <name>` is used to generate a fresh project.

Typically a project is defined in a `mix.exs` file, defining the varying options including dependencies,
tasks, and configuration paths.

With a defined project, mix tasks can be run from the command line:
- `mix compile` to compile the project
- `mix test` to run the test suite for the project
- `mix run` for specific commands in the project
- `mix help` lists available tasks including those defined in the mix file

### IEx

IEx is Elixir's interactive shell. It includes various helpers for various tasks and inspection. The list
of helpers can be accessed with `h()`.

Remote nodes may be connected to from the local session to interact with already running applications.

### ExUnit

ExUnit is the full test framework included with Elixir.

Tests are defined in script files, typically defined in the `test` directory named `<filename>_test.exs` for
each file in the `lib` directory.

Tests use `ExUnit.Case` to inject the API and define tests with the `test/2` macro. A `test/test_helper.exs`
file is responsible for setting up the test framework.

ExUnit allows for documentation to embed tests alongside the functions being documented.

```elixir
@doc """
Add 2 numbers together.

## Examples

  iex> add(1, 2)
  3

"""
def add(a, b) do
  a + b
end
```

## Processes

Processes form the core of state and concurrency in Elixir. The scheduler ensures all processes get equal
opportunity to run and spread the work across the entire system.

### Actor Model

Elixir is built on the actor model, a model of concurrency that separates operations into separate, isolated
processes the communicate by message passing.

### Lightweight

Each process is a lightweight unit separate from operating system processes and threads. It is not uncommon to
have tens or hundreds of thousands of processes running simultaneously on a single machine.

### Messaging

Each process has a mailbox used in the message passing communication between processes.

Messages are sent to processes by `send/2`, and the messages wait in the process's mailbox until retrieved with
`receive/1`.

### Keeping State

Being an immutable language, state cannot be kept by updating a map. Using recursion and messaging, a process
may be used to keep persistent state.

By entering a recursive loop in a process's `recieve/1` block, state may be manipulated and routed back through
the loop. The process can respond to messages requesting the state and updating the state to form the model of
persistence.

### Links

Processes are generally spawned as linked processes. Processes linked together propogate exit signals to other
linked processes.

A process may trap exits from other processess, allowing it to take action on the exits instead of exiting itself.
This mechanism forms the foundation of the system's fault tolerance.

## OTP

Originally the Open Telecom Platform, OTP has grown beyond being limited to a small range of applications. It
includes a wide array of tools and components for building a distributed, fault-tolerant system.

### GenServer

Abstracting the manual setup of a state-holding process, Elixir provides the GenServer behaviour module as a
foundational unit to build upon.

### Supervisor

A Supervisor manages the lifecycle of its GenServer children, defining their restart strategies, initial state, and
serving as an API.

### Supervision Trees

By allowing Supervisors to supervise other Supervisors, a supervision tree can be constructed to encapsulate
crashes and protect the rest of the system from unexpected circumstances.

This forms the "let it crash" philosophy. With proper separation of concerns and well-defined supervision
strategies, a GenServer with unexpected state can crash and be restarted with well-formed state in an attempt to
gracefully handle the situation. In the event the issue persists, the failures will begin to bubble up the
supervision tree until a good state is achieved or the entire application crashes.

#### `:ok`/`:error`

Many libraries provide pairs of functions that deal with more unpredictable operations. The `foo` form
returns `:ok`/`:error` tuples, and the `foo!` form raises execptions. This separation allows the developer
to choose how to deal with the exceptional circumstances.

### Umbrella Applications

One of the primary uses of applications is to separate supervision responsibilities. When a supervision tree can
be isolated from the remainder of the system, it can be broken out into a separate application to be joined
together under an umbrella application.

This provides a way to have separate concerns within the same code base but allow different managment strategies.

An example web server may include a set of distinct applications for its separate concerns within a single umbrella
application:
- web API
- database access
- encapsulated library of business logic
- event sourcing handlers

## Macros

Elixir is built with extensiblity in mind, down to the language itself.

Macros provide metaprogramming to the language. Portions of the native language are written with macros (including
`if`/`unless` which are macros around `cond`).

Macros provide a powerful mechanism to build domain specific languages, such as Phoenix's router, but they come at
a cost of language transparency.

A Phoenix GET HTTP route.
```elixir
get "/", PageController, :index
```

The expanded `match/5` function underlying the macro.
```elixir
def match(:get, "/", PageController, :index, [])
```

## Resources
- [Elixir Docs](https://elixir-lang.org/getting-started/introduction.html)
- [hexdocs Elixir](https://hexdocs.pm/elixir/)
- [Learn Elixir in Y minutes](https://learnxinyminutes.com/docs/elixir/)
- [Programming Elixir](https://pragprog.com/book/elixir13/programming-elixir-1-3)
- [Elixir In Action](https://www.manning.com/books/elixir-in-action)
