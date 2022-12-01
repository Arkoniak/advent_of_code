using HTTP
using ConfigEnv

dotenv()

aoc_input(year, day, session = ENV["AOC"]) = HTTP.get("https://adventofcode.com/$year/day/$day/input"; cookies = Dict("session" => session)).body |> String

open("input.txt", "w") do f
    print(f, aoc_input(2021, 4))
end
