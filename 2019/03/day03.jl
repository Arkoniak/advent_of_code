#################################
# Part 1

struct Dot
    x::Int
    y::Int
end

function Base.:+(d::Dot, dir::Char)
    if dir == 'R'
        return Dot(d.x + 1, d.y)
    elseif dir == 'L'
        return Dot(d.x - 1, d.y)
    elseif dir == 'U'
        return Dot(d.x, d.y + 1)
    elseif dir == 'D'
        return Dot(d.x, d.y - 1)
    end
end

Base.:+(d::Dot, dir::T) where T <:AbstractString = +(d, dir[1])
Base.:collect(d::Dot) = [d]

dist(d::Dot) = abs(d.x) + abs(d.y)

function generate_path(s)
    cur = Dot(0, 0)
    path = Vector{Dot}()
    s = split(s, ",")
    for move in s
        op = move[1]
        l = parse(Int, move[2:end])
        for i in 1:l
            cur += op
            push!(path, cur)
        end
    end
    
    return path
end

function part1()
    s = readlines("input.txt")
    p1 = Set(generate_path(s[1]))
    p2 = Set(generate_path(s[2]))

    println("Part 1: ", minimum(dist.(intersect(p1, p2))))
end

part1()

#######################################
# Part 2

function find_intersection_length(s1, s2)
    p1 = generate_path(s1)
    p2 = generate_path(s2)
    
    intersections = intersect(p1, p2)
    res = Vector{Int}()
    for x in intersections
        i1 = minimum(findall(p1 .== x))
        i2 = minimum(findall(p2 .== x))
        
        push!(res, i1 + i2)
    end
    
    return minimum(res)
end

function part2()
    s = readlines("input.txt")
    println("Part 2: ", find_intersection_length(s[1], s[2]))
end

part2()
