function get_data(file = "input.txt")
    readlines(joinpath(@__DIR__, file)) |> x -> collect.(x) |>
        x -> map.(y -> y == '#' ? 1 : 0, x) |> x -> hcat(x...) |> permutedims
end

cut_left(x, t) = x < t ? t : x
cut_right(x, t) = x > t ? t : x

function neighbours_cnt(grid, i, j)
    ul = cut_left.((i, j) .- (1, 1), 1)
    br = cut_right.((i, j) .+ (1, 1), size(data, 1))
    return sum(@view grid[ul[1]:br[1], ul[2]:br[2]]) - grid[i, j]
end

function makestep(grid)
    state = similar(grid)
    for x in CartesianIndices(grid)
        cnt = neighbours_cnt(grid, x[1], x[2])
        if ((grid[x] == 0) & (cnt == 3)) |
            ((grid[x] == 1) & ((cnt == 2) | (cnt == 3)))
            state[x] = 1
        else
            state[x] = 0
        end
    end
    state
end

function part1()
    data = get_data()
    state = data
    for i in 1:100
        state = makestep(state)
    end
    println("Part 1: $(sum(state))")
end

part1()

#########################################
# Part 2

function makestep2(grid)
    state = makestep(grid)
    n, m = size(grid)
    state[1, 1] = 1
    state[1, n] = 1
    state[n, 1] = 1
    state[n, m] = 1
    state
end

function part2()
    data = get_data()
    state = data
    for i in 1:100
        state = makestep2(state)
    end
    println("Part 1: $(sum(state))")
end

part2()
