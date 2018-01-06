c("fizzbuzz.exs", ".")

defmodule Demo do
  def print_list(list) do
    Enum.map(list, &IO.puts/1)
    list
  end
end
