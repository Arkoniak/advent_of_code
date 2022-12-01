########################################
# Part 1
########################################
function parseinput(filename)
    r1 = Dict{Int, Vector{Vector{Int}}}()
    r2 = Dict{Int, Char}()
    vc = String[]
    first = true
    for line in eachline(filename)
        if isempty(line)
            first = false
            continue
        end
        if first
            k, v = split(line, ":")
            k = parse(Int, k)
            if occursin(r"[0-9]", v)
                v = split.(strip.(split(v, "|")), " ") .|> x -> parse.(Int, x)
                r1[k] = v
            else
                r2[k] = strip(v)[2]
            end
        else
            push!(vc, line)
        end
    end

    return r1, r2, vc
end

function validate(r1, r2, rid, line, i)
    if haskey(r1, rid)
        i0 = i
        for rr in r1[rid]
            failed = false
            for id in rr
                i <= length(line) || (failed = true; break)
                t, i = validate(r1, r2, id, line, i)
                t || (failed = true; break)
            end
            failed || return true, i
            i = i0
        end
        return false, i
    else
        return line[i] == r2[rid], i + 1
    end
    
    return false, i
end

function validate(r1, r2, line)
    t, i = validate(r1, r2, 0, line, 1)
    t && i == length(line) + 1
end

r1, r2, vc = parseinput("input.txt")
println("Part1: ", count(x -> validate(r1, r2, x), vc))

########################################
# Part 2
########################################
r1, r2, vc = parseinput("input_test3.txt")
r1[8] = [[42], [42, 8]]
r1[11] = [[42, 31], [42, 11, 31]]
# r1[8] = [[42]]
# r1[11] = [[42, 31]]
println("Part1: ", count(x -> validate(r1, r2, x), vc))
validate(r1, r2, 11, "babbbbaabbbbbabbbbbbaabaaabaaa", 1)
validate(r1, r2, 42, "baabbbbbabbbbbbaabaaabaaa", 1)
validate(r1, r2, 31, "abaaa", 1)


########################################
# Misc
########################################

########################################
# Tests
########################################
module Tests
using ReTest
# using Main: part1

@testset "Misc" begin
end

@testset "Part 1" begin
end

@testset "Part 2" begin
end

end # module

Tests.runtests(Tests)
