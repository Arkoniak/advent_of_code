function calc(v)
    c = v[end]
    while !iszero(v)
        v = diff(v)
        c += v[end]
    end

    return c
end

sum(line -> calc(parse.(Int, split(line, " "))), eachline("input.txt"))

function calc2(v)
    c, s = v[begin], -1
    while !iszero(v)
        v = diff(v)
        c += v[begin] * s
        s = -s
    end

    return c
end
sum(line -> calc2(parse.(Int, split(line, " "))), eachline("input.txt"))
