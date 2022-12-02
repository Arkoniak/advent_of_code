# Part 1
function f()
    v = Int[]
    s = 0
    for row in eachline("input.txt")
        if isempty(row)
            push!(v, s)
            s = 0
        else
            s += parse(Int, row)
        end
    end

    return maximum(v)
end

f()

# Part 2
function f()
    v = Int[]
    s = 0
    for row in eachline("input.txt")
        if isempty(row)
            push!(v, s)
            s = 0
        else
            s += parse(Int, row)
        end
    end

    partialsort!(v, 3, rev = true)
    return sum(v[1:3])
end

f()
