########################################
# Part 1
########################################
range(s) = parse.(Int, split(s, " "))

function seeding(s)
    lines = split(s, "\n", keepempty = false)
    m = match(r"([a-z]+)-to-([a-z]+) map:", lines[1])
    fname = Symbol(m[2])
    func = :(function $fname(x) end)
    
    sr, ds, rg = range(lines[2])
    ds2 = ds + rg
    cond = :(if x >= $ds && x < $ds2 x - $ds + $sr end)
    branch = cond.args
    for id in 3:length(lines)
        sr, ds, rg = range(lines[id])
        ds2 = ds + rg
        c = Expr(:elseif, :(x >= $ds && x < $ds2), :(x - $ds + $sr))
        push!(branch, c)
        branch = c.args
    end
    push!(branch, :(x))
    push!(func.args[2].args, cond)
    return esc(func), fname
end

macro build(filename)
    info = split(read(open(filename, "r"), String), "\n\n")
    expr = :(begin end)
    res = :(x)
    for id in 2:length(info)
        f, fname = seeding(info[id])
        res = :($fname($res))
        push!(expr.args, f)
    end

    push!(expr.args, esc(:(resloc(x) = $res)))

    seeds = parse.(Int, split(info[1], " ")[2:end])
    seeds2 = conv2ll(seeds)
    push!(expr.args, esc(:(day1() = minimum(resloc, $seeds))))
    return expr
end

@macroexpand @build "test_input.txt"

day1()

########################################
# Part 2
########################################

conv2ll(v)

resloc(13)

location(humidity(temperature(light(water(fertilizer(soil(79)))))))

dump(:(foo(bar(1))))

dump(:(begin
    function foo() end
    function bar() end
       end))

v = parse.(Int, split(split(read(open("test_input.txt", "r"), String), "\n\n")[1], " ")[2:end])

@macroexpand @seeding """
seed-to-soil map:
50 98 2
52 50 48
"""

@seeding """
seed-to-soil map:
50 98 2
52 50 48
"""

soil(13)

dump(:(function foo(x) 
        x + 1
    end
   ))

dump(:(if x > 1
        1
    elseif x < 2
        2
    elseif x > 5
        4
    else 
        3
    end))
