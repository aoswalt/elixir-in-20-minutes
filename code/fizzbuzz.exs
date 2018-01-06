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
  Generate a list of values from 1 to the specified positive integer limit
  following the fizzbuzz rules.
  """
  def generate(limit) when is_integer(limit) and limit > 0 do
    count(1, limit)
  end

  def generate(limit) when is_integer(limit) do
    {:error, "Expected a positive integer"}
  end

  def generate(_) do
    {:error, "Expected a positive integer to count to"}
  end


  defp count(n, limit, list \\ [])

  defp count(n, limit, list) when n > limit do
    # list is built backwards, so reversing to natural order for final result
    Enum.reverse(list)
  end

  defp count(n, limit, list) when rem(n, 3) == 0 and rem(n, 5) == 0 do
    count(n + 1, limit, ["fizzbuzz" | list])
  end

  defp count(n, limit, list) when rem(n, 5) == 0 do
    count(n + 1, limit, ["buzz" | list])
  end

  defp count(n, limit, list) when rem(n, 3) == 0 do
    count(n + 1, limit, ["fizz" | list])
  end

  defp count(n, limit, list) do
    count(n + 1, limit, [n | list])
  end
end
