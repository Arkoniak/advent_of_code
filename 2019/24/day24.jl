##############################
# Part 1

function read_data(inp = "input.txt")
    arr = hcat(collect.(readlines(inp))...)
    arr = map(x -> x == '.' ? 0 : 1, arr)
    arr0 = zeros(Int, size(arr)[1] + 2, size(arr)[2] + 2)
    arr0[2:(size(arr)[1] + 1), 2:(size(arr)[2] + 1)] .= arr
    arr0
end

function tick(arr)
    arr0 = copy(arr)
    dirs = [[1, 0], [0, 1], [-1, 0], [0, -1]]
    for i in 2:6, j in 2:6
        adj = sum([arr[i + d[1], j + d[2]] for d in dirs])
        if arr[i, j] == 0 && (adj == 1 || adj == 2) arr0[i, j] = 1 end
        if arr[i, j] == 1 && adj != 1 arr0[i, j] = 0 end
    end
    
    arr0
end

function biodiversity(arr)
    arr = arr[2:6, 2:6]
    res = 0
    for i in 1:5
        for j in 1:5
            res += arr[i, j] * 2^((j - 1)*5 + i - 1)
        end
    end
    res
end

function part1()
    arr = read_data()
    bios = Set{Int}()
    while true
        bio = biodiversity(arr)
        if !(bio in bios)
            push!(bios, bio)
        else
            println("Part 1: ", bio)
            break
        end
        arr = tick(arr)
    end
end

part1()

###########################################
# Part 2

struct Cell
    x::Int
    y::Int
    d::Int
end

function neighbours(c::Cell)
    res = Cell[]
    dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    for d in dirs
        x1 = c.x + d[1]
        y1 = c.y + d[2]
        if x1 == 0
            push!(res, Cell(2, 3, c.d - 1))
        elseif x1 == 6
            push!(res, Cell(4, 3, c.d - 1))
        elseif y1 == 0
            push!(res, Cell(3, 2, c.d - 1))
        elseif y1 == 6
            push!(res, Cell(3, 4, c.d - 1))
        elseif x1 == 3 && y1 == 3
            if c.x == 2
                append!(res, [
                        Cell(1, 1, c.d + 1),
                        Cell(1, 2, c.d + 1),
                        Cell(1, 3, c.d + 1),
                        Cell(1, 4, c.d + 1),
                        Cell(1, 5, c.d + 1)
                        ])
            elseif c.x == 4
                append!(res, [
                        Cell(5, 1, c.d + 1),
                        Cell(5, 2, c.d + 1),
                        Cell(5, 3, c.d + 1),
                        Cell(5, 4, c.d + 1),
                        Cell(5, 5, c.d + 1)
                        ])
            elseif c.y == 2
                append!(res, [
                        Cell(1, 1, c.d + 1),
                        Cell(2, 1, c.d + 1),
                        Cell(3, 1, c.d + 1),
                        Cell(4, 1, c.d + 1),
                        Cell(5, 1, c.d + 1)
                        ])
            else
                append!(res, [
                        Cell(1, 5, c.d + 1),
                        Cell(2, 5, c.d + 1),
                        Cell(3, 5, c.d + 1),
                        Cell(4, 5, c.d + 1),
                        Cell(5, 5, c.d + 1)
                        ])
            end
        else
            push!(res, Cell(x1, y1, c.d))
        end
    end
    res
end

function read_data2(inp = "input.txt")
    arr = hcat(collect.(readlines(inp))...)
    arr0 = Set{Cell}()
    for i in 1:5, j in 1:5
        if arr[i, j] == '#' push!(arr0, Cell(i, j, 0)) end
    end
    arr0
end

function tock(arr)
    res = Set{Cell}()
    mind, maxd = [f([c.d for c in arr]) for f in (x -> minimum(x) - 1, x -> maximum(x) + 1)]
    for d in mind:maxd
        for i in 1:5, j in 1:5
            if i == 3 && j == 3 continue end
            c = Cell(i, j, d)
            adj = sum([neighbour in arr for neighbour in neighbours(c)])
            if (c in arr) && (adj == 1) push!(res, c) end
            if !(c in arr) && ((adj == 1) || (adj == 2)) push!(res, c) end
        end
    end
    
    res
end

function part2()
    arr = read_data2("input.txt")
    for i in 1:200
        arr = tock(arr)
    end
    println("Part 2: ", length(arr))
end

part2()
