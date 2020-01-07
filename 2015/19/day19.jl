##########################
# Part 1

cd(dirname(@__FILE__))

function dissolve(molecule)
    res = Vector{Symbol}()
    idx = 1
    while true
        if (idx <= length(molecule) - 1) && (molecule[idx + 1] in 'a':'z')
            push!(res, Symbol(molecule[[idx, idx + 1]]))
            idx += 2
        else
            push!(res, Symbol(molecule[idx]))
            idx += 1
        end
        if idx > length(molecule) return res end
    end
end

function part1()
    ss = readlines("input.txt")

    d = Dict{Symbol, Vector{Vector{Symbol}}}()

    for s in ss
        if s == "" break end
        m = match(r"(.*) => (.*)", s)
        elem = get!(d, Symbol(m[1]), [])
        push!(elem, dissolve(m[2]))
    end

    molecule = dissolve(ss[end])

    println("Part 1: ", length(mutations(molecule, d)))
end

part1()

############################
# Part 2

function part2()
    ss = readlines("input.txt")
    molecule = ss[end]

    res = length(dissolve(molecule)) - length(findall("Rn", molecule)) - length(findall("Ar", molecule)) - 2*length(findall("Y", molecule)) - 1
    println("Part 2: ", res)
end

part2()
