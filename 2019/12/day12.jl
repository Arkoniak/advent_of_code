####################################
# Part 1
struct Planet
    pos::Vector{Int}
    vel::Vector{Int}
end

Planet(s::String) = Planet(parse.(Int, match(r"x=([\-0-9]+).*y=([\-0-9]+).*z=([\-0-9]+)", s).captures), zeros(Int, 3))
veldiff!(p1::Planet, p2::Planet) = (p1.vel .+= sign.(p2.pos .- p1.pos))
shift!(p::Planet) = (p.pos .+= p.vel)
energy(p::Planet) = sum(abs.(p.pos))*sum(abs.(p.vel))

function part1()
    planets = Planet.(readlines("input.txt"))

    for i in 1:1000
        for p1 in planets, p2 in planets veldiff!(p1, p2) end
        for p in planets shift!(p) end
    end

    println("Part 1: ", sum(energy.(planets)))
end

part1()

####################################
# Part 2

mutable struct Planet1
    pos::Int
    vel::Int
end

veldiff!(p1::Planet1, p2::Planet1) = (p1.vel += sign(p2.pos - p1.pos))
shift!(p::Planet1) = (p.pos += p.vel)

Base.:copy(p::Planet1) = Planet1(p.pos, p.vel)
equal(p1::Planet1, p2::Planet1) = p1.pos == p2.pos && p1.vel == p2.vel

function find_cycle(planets::Vector{Planet1})
    init = copy.(planets)
    steps = 1
    while true
        for p1 in planets, p2 in planets veldiff!(p1, p2) end
        for p in planets shift!(p) end
        
        if all(equal.(planets, init)) return steps end
        steps += 1
    end
end

function part2()
    planets = Planet.(readlines("input.txt"))
	println("Part 2: ", lcm([find_cycle([Planet1(p.pos[i], p.vel[i]) for p in planets]) for i in 1:3]...))
end

part2()
