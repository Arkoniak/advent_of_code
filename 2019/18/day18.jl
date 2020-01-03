#####################################################
# Part 1

struct Maze
    m::Array{Char, 2}
end

Maze(s::Vector{T}) where T <: String = Maze(hcat(collect.(s)...))

struct MazeState
    collected_keys::Set{Char}
    pos::Char
end

find_element(m::Maze, el::Char) = get(CartesianIndices(m.m)[m.m .== el], 1, nothing)
find_elements(m::Maze, els) = [(x, find_element(m, x)) for x in els if find_element(m, x) != nothing]
mkeys(m::Maze) = Dict(find_elements(m, 'a':'z'))
doors(m::Maze) = Dict(find_elements(m, 'A':'Z'))
hero(m::Maze) = find_element(m, '@')
heros(m::Maze) = CartesianIndices(m.m)[m.m .== '@']

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
    res1 = Char[]
    res2 = Char[]
    current = start
    dirs = CartesianIndex.([(0, 1), (1, 0), (0, -1), (-1, 0)])
    while current != finish
        for d in dirs
            if arr[current + d] + 1 == arr[current]
                current = current + d
                if maze.m[current] in collect('A':'Z') push!(res1, lowercase(maze.m[current])) end
                if maze.m[current] in collect('a':'z') push!(res2, lowercase(maze.m[current])) end
                break
            end
        end
    end
                
    res1, res2
end

function build_info(m::Maze)
    res = Dict()
    for (k, loc) in mkeys(m)
        arr = floodfill(m, loc)
        res[k] = Dict([(k1, (arr[v1], get_door_keys(m, arr, v1, loc)...)) for (k1, v1) in mkeys(m)])
    end
    
    arr = floodfill(m, hero(m))
    res['@'] = Dict([(k1, (arr[v1], get_door_keys(m, arr, v1, hero(m))...)) for (k1, v1) in mkeys(m)])
    
    res
end

function part1(inp = "input.txt")
    m = Maze(readlines(inp))
    nkeys = length(mkeys(m)) + 1
    info = build_info(m)
    memo = Dict{Tuple{Set{Char},Char}, Int}()
    s0 = (Set(Char[]), '@')
    memo[s0] = 0
    acc = [s0]
    while !isempty(acc)
        state = popfirst!(acc)
        for (k, v) in info[state[2]]
            if !(k in state[1])
                if length(intersect(v[2], state[1])) == length(v[2])
                    new_keys = union(state[1], k, v[3])
                    new_state = (new_keys, k)
                    if !(new_state in keys(memo)) || memo[new_state] > memo[state] + v[1]
                        memo[new_state] = memo[state] + v[1]
                        push!(acc, new_state)
                    end
                end
            end
        end
    end
    
    println("Part 1: ", minimum([v for (k, v) in memo if length(k[1]) == 26]))
end

part1()

#######################################
# Part 2

function prep_maze()
    m = Maze(readlines("input.txt"))
    h = hero(m)
    m.m[h[1], h[2]] = '#'
    m.m[h[1] + 1, h[2]] = '#'
    m.m[h[1] - 1, h[2]] = '#'
    m.m[h[1], h[2] + 1] = '#'
    m.m[h[1], h[2] - 1] = '#'
    m.m[h[1] + 1, h[2] + 1] = '1'
    m.m[h[1] - 1, h[2] + 1] = '2'
    m.m[h[1] + 1, h[2] - 1] = '3'
    m.m[h[1] - 1, h[2] - 1] = '4'
    
    m
end

function part2()
    m = prep_maze()
    res = Dict()
    for (k, loc) in mkeys(m)
        arr = floodfill(m, loc)
        res[k] = Dict([(k1, (arr[v1], get_door_keys(m, arr, v1, loc)...)) for (k1, v1) in mkeys(m) if (arr[v1] != -1) && (arr[v1] != 0)])
    end
    
    for hh in '1':'4'
        arr = floodfill(m, find_element(m, hh))
        res[hh] = Dict([(k1, (arr[v1], get_door_keys(m, arr, v1, find_element(m, hh))...)) for (k1, v1) in mkeys(m) if arr[v1] != -1])
    end
    
    info = res
    memo = Dict{Tuple{Set{Char}, Vector{Char}}, Int}()
    s0 = (Set(Char[]), collect('1':'4'))
    memo[s0] = 0
    acc = [s0]
    while !isempty(acc)
        state = popfirst!(acc)
        for (i, loc) in enumerate(state[2])
            for (k, v) in info[loc]
                if !(k in state[1])
                    if length(intersect(v[2], state[1])) == length(v[2])
                        new_keys = union(state[1], k, v[3])
                        new_pos = copy(state[2])
                        new_pos[i] = k
                        new_state = (new_keys, new_pos)
                        if !(new_state in keys(memo)) || memo[new_state] > memo[state] + v[1]
                            memo[new_state] = memo[state] + v[1]
                            push!(acc, new_state)
                        end
                    end
                end
            end
        end
    end
    
    println("Part 2: ", minimum([v for (k, v) in memo if length(k[1]) == 26]))
end

part2()
