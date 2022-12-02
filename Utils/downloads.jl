using HTTP
using ConfigEnv

dotenv()

aoc_input(year, day, session = ENV["AOC"]) = HTTP.get("https://adventofcode.com/$year/day/$day/input"; cookies = Dict("session" => session)).body |> String

function main()
    open("input.txt", "w") do f
        print(f, aoc_input(ARGS[1], ARGS[2]))
    end
end

main()
