###################################
# Part 1

function part1()
    cd(dirname(@__FILE__))
    data = read("input.txt", String)

    res = 0
    while true
        m = match(r"(-?[0-9]+)(.*)", data)
        if m == nothing break end
        res += parse(Int, m[1])
        data = m[2]
    end

    println("Part 1: ", res)
end

part1()

###################################
# Part 2
import JSON

function part2()
    cd(dirname(@__FILE__))
    states = []
    push!(states, JSON.parse(read("input.txt", String)))
    res = 0
    while !isempty(states)
        state = pop!(states)
        if state isa Number
            res += state
        elseif state isa Vector
            append!(states, state)
        elseif state isa Dict
            if !("red" in values(state))
                append!(states, values(state))
            end
        end
    end

    println("Part 2: ", res)
end

part2()
