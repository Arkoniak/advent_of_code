###########################
# Part 1

struct Deer
    name::String
    fly::Int
    period::Int
    rest::Int
end

function distance(d::Deer, t)
    d.fly*((t ÷ (d.period + d.rest))*d.period + min(mod(t, d.period + d.rest), d.period))
end

function part1()
    ss = readlines("input.txt")
    deers = Deer[]
    for s in ss
        m = match(r"^([^ ]+) [^0-9]*([0-9]+) km/s for ([0-9]+).*rest[^0-9]*([0-9]+)", s)
        push!(deers, Deer(m[1], parse.(Int, m.captures[2:end])...))
    end

    println("Part 1: ", maximum(map(x -> distance(x, 2503), deers)))
end

part1()

##########################
# Part 2

function part2()
    ss = readlines("input.txt")
    deers = Deer[]
    for s in ss
        m = match(r"^([^ ]+) [^0-9]*([0-9]+) km/s for ([0-9]+).*rest[^0-9]*([0-9]+)", s)
        push!(deers, Deer(m[1], parse.(Int, m.captures[2:end])...))
    end

    res = zeros(Int, length(deers))
    for i in 1:2503
        dist = [distance(d, i) for d in deers]
        m = maximum(dist)
        res += dist .== m
    end

    println("Part 2: ", maximum(res))
end

part2()
