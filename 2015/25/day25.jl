function next(t::Tuple{Int, Int})
    t[1] == 1 ? (t[2] + 1, 1) : (t[1] - 1, t[2] + 1)
end

function next(v::Int)
    rem(v * 252533, 33554393)
end

function part1()
    v = 20151125
    t = (1, 1)
    while true
        t = next(t)
        v = next(v)
        t == (2981, 3075) && return v
    end
end

part1()
