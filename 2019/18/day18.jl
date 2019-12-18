struct Maze
    m::Array{Char, 2}
end

Maze(s::Vector{T}) where T <: String = Maze(hcat(collect.(s)...))

find_element(m::Maze, el::Char) = get(CartesianIndices(m.m)[m.m .== el], 1, nothing)
find_elements(m::Maze, els) = [(x, find_element(m, x)) for x in els if find_element(m, x) != nothing]
mkeys(m::Maze) = Dict(find_elements(m, 'a':'z'))
doors(m::Maze) = Dict(find_elements(m, 'A':'Z'))
hero(m::Maze) = find_element(m, '@')

function neighbours(m::Maze, l)
    dirs = CartesianIndex.([(0, 1), (1, 0), (0, -1), (-1, 0)])
    [l + d for d in dirs[[m.m[l + d] != '#' for d in dirs]]]
end

function floodfill(m::Maze, start)
    arr = fill(-1, size(m.m)...)
    arr[start] = 0
    acc = [start]
    while !isempty(acc)
        loc = popfirst!(acc)
        for n in neighbours(m, loc)
            if arr[n] == -1 || arr[n] > arr[loc] + 1
                arr[n] = arr[loc] + 1
                push!(acc, n)
            end
        end
    end
    
    arr
end

function get_door_keys(maze::Maze, arr, start, finish)
    println(start, finish)
    println(arr[start])
    println(arr[finish])
    res = Char[]
    current = start
    dirs = CartesianIndex.([(0, 1), (1, 0), (0, -1), (-1, 0)])
    cnt = 0
    while current != finish
        for d in dirs
            println(d, current, " : ", arr[current])
            if arr[current + d] + 1 == arr[current]
                current = current + d
                if maze.m[current] in collect('A':'Z') push!(res, lowercase(maze.m[current])) end
                break
            end
        end
    end
end

function build_info(m::Maze)
    res = Dict()
    for (k, loc) in mkeys(m)
        println(k)
        arr = floodfill(m, loc)
        res[k] = Dict([(k1, (arr[v1], get_door_keys(m, arr, v1, loc))) for (k1, v1) in mkeys(m)])
    end
    
    arr = floodfill(m, hero(m))
    res['@'] = Dict([(k1, (arr[v1], Char[])) for (k1, v1) in mkeys(m)])
    
    res
end

m = Maze(readlines("test1.txt"))
println.(readlines("test1.txt"))

build_info(m)
