function get_td1(filename)
    s = read(open(filename, "r"), String)
    s = split(s, "\n", keepempty = false)
    map(x -> parse.(Int, x), collect(zip(split.(s, r"\s+")...))[2:end])
end

function get_td2(filename)
    s = read(open(filename, "r"), String)
    s = split(s, "\n", keepempty = false)
    map(x -> parse.(Int, filter.(y -> !isspace(y), x)), collect(zip(split.(s, ":")...))[2:end])
end

function minmax(p)
    d = sqrt(p[1]*p[1] - 4*p[2])
    x1 = ceil(Int, (p[1] - d)/2)
    x1 = x1*(p[1] - x1) == p[2] ? x1 + 1 : x1
    x2 = floor(Int, (p[1] + d)/2)
    x2 = x2*(p[1] - x2) == p[2] ? x2 - 1 : x2

    return x2 - x1 +1
end

function day6(f, filename)
    v = f(filename)
    prod(minmax.(v))
end

day6_1(filename) = day6(get_td1, filename)
day6_2(filename) = day6(get_td2, filename)

day6_1("input.txt")
day6_2("input.txt")
