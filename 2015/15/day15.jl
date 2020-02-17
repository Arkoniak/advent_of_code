###########################
# Part 1
using LinearAlgebra

function part1data()
    data = readlines(joinpath(@__DIR__, "input.txt"))
    ingredients = map(data) do datum
        m = match(r"^(\w+): (.*)$", datum)
        split(m[2], ",") |> x -> strip.(x) |> x -> split.(x, " ") |>
            x -> getindex.(x, 2) |> x -> parse.(Int, x)
    end

    return map(x -> x[1:4], ingredients) |> x -> hcat(x...)
end

function apply_recipe(data, v)
    data * v |> x -> *(ifelse.(x .< 0, 0, x)...)
end

function part1()
    ingredients = part1data()
    res = 0
    for i in 0:100
        for j in 0:100 - i
            for k in 0:100 - i - j
                ires = apply_recipe(ingredients, [i, j, k, 100 - i - j - k])
                res = res < ires ? ires : res
            end
        end
    end

    println("Part 1: ", res)
end

part1()

#####################################

function part2data()
    data = readlines(joinpath(@__DIR__, "input.txt"))
    ingredients = map(data) do datum
        m = match(r"^(\w+): (.*)$", datum)
        split(m[2], ",") |> x -> strip.(x) |> x -> split.(x, " ") |>
            x -> getindex.(x, 2) |> x -> parse.(Int, x)
    end

    return (map(x -> x[1:4], ingredients) |> x -> hcat(x...), map(x -> x[5], ingredients))
end

function part2()
    ingredients, cals = part2data()
    res = 0
    for i in 0:100
        for j in 0:100 - i
            for k in 0:100 - i - j
                v = [i, j, k, 100 - i - j - k]
                if cals ⋅ v == 500
                    ires = apply_recipe(ingredients, [i, j, k, 100 - i - j - k])
                    res = res < ires ? ires : res
                end
            end
        end
    end

    println("Part 2: ", res)
end

part2()
