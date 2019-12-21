function teleports(m::Maze)
    entry_points = Dict{Symbol, Vector{CartesianIndex}}()
    nest_level = Dict{CartesianIndex, Int}()
    dirs = CartesianIndex.([(0, 1), (1, 0), (0, -1), (-1, 0)])
    h, w = size(m.m)
    for t in 'A':'Z'
        for loc in find_element(m, t)
            entry_point = CartesianIndex(-1, -1)
            for d in dirs
                if m.m[loc + d] in collect('A':'Z')
                    global teleport_name = Symbol(join(sort([t, m.m[loc + d]])))
                elseif m.m[loc + d] == '.'
                    entry_point = loc + d
                end
            end
            if entry_point != CartesianIndex(-1, -1)
                points = get!(entry_points, teleport_name, [])
                push!(points, entry_point)
                if (loc[1] == 3) || (loc[1] == h - 2) || (loc[2] == 3) || (loc[2] == w - 2)
                    nest_level[entry_point] = -1
                else
                    nest_level[entry_point] = 1
                end
            end
        end
    end
    
    tlps = Dict{CartesianIndex, CartesianIndex}()
    for (k, v) in entry_points
        if k in [:AA, :ZZ] continue end
        tlps[v[1]] = v[2]
        tlps[v[2]] = v[1]
    end
    
    entry_points, nest_level, tlps
end

function floodfill(m::Maze, tlps, nest_levels, start, finish)
    arr = Dict{Tuple{CartesianIndex, Int}, Int}()
    arr[start] = 0
    acc = [start]
    VERYBIG = 2^63 - 1
    arr[finish] = VERYBIG
    while !isempty(acc)
        loc, level = popfirst!(acc)
        for n in neighbours(m, loc)
            val = get!(arr, (n, level), VERYBIG)
            if (val > arr[(loc, level)] + 1) && (arr[(loc, level)] + 1 < arr[finish])
                arr[(n, level)] = arr[(loc, level)] + 1
                push!(acc, (n, level))
                if (n in keys(tlps))
                    if (get!(arr, (tlps[n], level + nest_levels[n]), VERYBIG) > arr[(loc, level)] + 2) &&
                            (arr[(loc, level)] + 2 < arr[finish]) &&
                            (level + nest_levels[n] >= 0)
                        arr[(tlps[n], level + nest_levels[n])] = arr[(loc, level)] + 2
                        push!(acc, (tlps[n], level + nest_levels[n]))
                    end
                end
            end
        end
    end
    
    arr
end

function part2(inp = "input.txt")
    m = Maze(readlines(inp))
    # show_maze(m)
    res = teleports(m)
    ffm = floodfill(m, res[3], res[2], (res[1][:AA][1], 0), (res[1][:ZZ][1], 0))
    println("Part 2: ", ffm[(res[1][:ZZ][1], 0)])
end
