####################################################
# Part 1

function get_data()
    joinpath(@__DIR__, "input.txt") |> readlines |> x -> parse.(Int, x)
end

function make_eggnog(data, capacity = 150, state = Int[], idx = 0, result::Vector{Vector{Int}} = Vector{Vector{Int}}())
    # If current state larger than capacity we silently ignore it
    if sum(data[state]) < capacity
        for i in idx+1:length(data)
            make_eggnog(data, capacity, vcat(state, [i]), i, result)
        end
    elseif sum(data[state]) == capacity
        push!(result, state)
    end

    result
end

function part1()
    data = get_data()
    println("Part 1: $(length(make_eggnog(data)))")
end

part1()

function part2()
    data = get_data()
    smallest = minimum(length.(make_eggnog(data)))
    result = filter(x -> length(x) == 4, res) |> length
    println("Part 2: $(result)")
end

part2()
