############################
# Part 1

function inc(c::Char)
    c += 1
    c > 'z' ? ('a', false) : (c, true)
end

function fix_iol(x)
    idx = findfirst(c -> c in ['i', 'o', 'l'], x)
    if idx != nothing
        x[idx] += 1
        for i in (idx+1):length(x)
            x[i] = 'a'
        end
    end
    x
end

function inc!(x::Vector{Char})
    for i in length(x):-1:1
        c1, brk = inc(x[i])
        x[i] = c1
        if brk break end
    end
    fix_iol(x)
end

function three_incr(x)
    for y in zip(x[1:(end-2)], x[2:(end-1)], x[3:end])
        if (y[1] == y[2] - 1) && (y[2] == y[3] - 1) return true end
    end
    false
end

two_pairs(x) = length(Set([y[1] for y in zip(x[1:(end - 1)], x[2:end]) if y[1] == y[2]])) >= 2

function next_password(x)
    x = collect(x)
    inc!(x)
    while !(three_incr(x) && two_pairs(x))
        inc!(x)
    end
    join(x)
end

function part1()
    x = "vzbxkghb"
    println("Part 1: ", next_password(x))
end

part1()

######################################
# Part 2

function part2()
    x = "vzbxkghb"
    x = next_password(x)
    println("Part 2: ", next_password(x))
end

part2()
