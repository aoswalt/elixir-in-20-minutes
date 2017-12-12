defmodule FizzBuzz do
  @moduledoc """
  An application of the fizzbuzz counting game.

  Count incrementally with the following rules:
  * If the number is divisible by 3, replace it with "fizz"
  * If the number is divisible by 5, replace it with "buzz"
  * If the number is divisible by 3 and by 5, replace it with "fizzbuzz"
  * Otherwise, keep the number
  """

  @doc """
  Prints 1 to the specified positive integer limit following the fizzbuzz rules.
  """
  def count_to(limit)

  def count_to(limit) when is_integer(limit) and limit > 0 do
    print(1, limit)
  end

  def count_to(limit) when is_integer(limit) do
    IO.puts "Expected a positive integer"
    :ok
  end

  def count_to(_) do
    IO.puts "Expected a positive integer to count to"
    :ok
  end


  defp print(n, limit) when n > limit do
    :ok
  end

  defp print(n, limit) when rem(n, 3) == 0 and rem(n, 5) == 0 do
    IO.puts "fizzbuzz"
    print(n + 1, limit)
  end

  defp print(n, limit) when rem(n, 5) == 0 do
    IO.puts "buzz"
    print(n + 1, limit)
  end

  defp print(n, limit) when rem(n, 3) == 0 do
    IO.puts "fizz"
    print(n + 1, limit)
  end

  defp print(n, limit) do
    IO.puts n
    print(n + 1, limit)
  end
end
