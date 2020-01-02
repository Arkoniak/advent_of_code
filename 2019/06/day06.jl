##############################
# Part 1

function conv2graph(data)
    graph = Dict{String, Vector{String}}()  # x is being orbited by [y, z]
    for orbit in data
        m = match(r"^([^\)]+)\)(.*)$", orbit)
        if m[1] in keys(graph)
            push!(graph[m[1]], m[2])
        else
            graph[m[1]] = [m[2]]
        end
    end
    
    graph
end

function orbits(graph, node, w)
    if node in keys(graph)
        return w + sum(map(x -> orbits(graph, x, w + 1), graph[node]))
    else
        return w
    end
end

function part1()
    graph = conv2graph(readlines("input.txt"))
    println("Part 1: ", orbits(graph, "COM", 0))
end

part1()

##############################
# Part 2

function conv2bigraph(data)
    graph = Dict{String, Vector}()  # x is being orbited by [y, z]
    for orbit in data
        m = match(r"^([^\)]+)\)(.*)$", orbit)
        m = string.(m.captures)
        if m[1] in keys(graph)
            push!(graph[m[1]][1], m[2])
        else
            graph[m[1]] = [[m[2]], -1]
        end
        
        if m[2] in keys(graph)
            push!(graph[m[2]][1], m[1])
        else
            graph[m[2]] = [[m[1]], -1]
        end
    end
    
    graph
end

function fillpath!(graph, node, weight)
    if (graph[node][2] == -1) || (graph[node][2] > weight)
        graph[node][2] = weight
        for n2 in graph[node][1]
            fillpath!(graph, n2, weight + 1)
        end
    end
end

function part2()
		graph = conv2bigraph(readlines("input.txt"))
		fillpath!(graph, "YOU", 0)
		println("Part 2: ", graph["SAN"][2] - 2)
end

part2()
