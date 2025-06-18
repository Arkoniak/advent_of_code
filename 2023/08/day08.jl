macro cycle(filename)
    strs = split(read(open(filename, "r"), String), "\n", keepempty = false)
    st = strs[1]
    ex = :(begin end)
    for c in st
        push!(ex.args, :(el = $(Symbol(c))(el); n += 1; el == "ZZZ" && return el, n))
    end
    ex = quote begin 
            LD, RD = create_dicts($(strs[2:end]))
            let LD = LD, RD = RD
                L(el) = LD[el]
                R(el) = RD[el]
            function cycle(el, n = 0)
                $ex
                return el, n
            end
            
            el, n = "AAA", 0
            while el != "ZZZ"
                el, n = cycle(el, n)
            end

            println(n)
            end
        end
    end

    return esc(ex)
end

@macroexpand @cycle "test_input.txt"
@cycle "test_input2.txt"
@cycle "input.txt"

cycle("AAA")
dump(@macroexpand @cycle "test_input.txt")


function create_dicts(strs)
    LD = Dict{String, String}()
    RD = Dict{String, String}()
    for s in strs
        m = match(r"([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)", s)

        LD[m[1]] = m[2]
        RD[m[1]] = m[3]
    end

    return LD, RD
end

create_dicts(split(read(open("test_input.txt", "r"), String), "\n", keepempty = false)[2:end])
split(read(open("test_input.txt", "r"), String), "\n", keepempty = false)[2:end]
m = match(r"([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)", "AAA = (BBB, CCC)")
m[1]
m[2]
m[3]

ex = :(function cycle(el, n = 0) return 1; return 2; end)
dump(ex)


ex = :(if R(el, n) == "ZZZ" return n elseif true return 0 elseif false return 5 end)
dump(ex)
