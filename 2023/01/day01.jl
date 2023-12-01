macro dig2(s)
    filt = filter(c -> isdigit(c), s)
    s0 = Meta.parse("$(filt[begin]filt[end])")
    return esc(:($s0))
end

macro task1(input_file)
    expr = :(+())
    for line in eachline(input_file)
        push!(expr.args, :(@dig2($line)))
    end
    return esc(expr)
end

@task1 "test_input.txt"
@task1 "input.txt"

macro dig2a(s)
    s1 = replace(s, "one" => "1",
               "two" => "2",
               "three" => "3",
               "four" => "4",
               "five" => "5",
               "six" => "6",
               "seven" => "7",
               "eight" => "8",
               "nine" => "9")
    s2 = replace(reverse(s), "eno" => 1,
                             "owt" => 2,
                             "eerht" => 3,
                             "ruof" => 4,
                             "evif" => 5,
                             "xis" => 6,
                             "neves" => 7,
                             "thgie" => 8,
                             "enin" => 9)
    s = s1*reverse(s2)
    filt = filter(c -> isdigit(c), s)
    s0 = Meta.parse("$(filt[begin]filt[end])")
    return esc(:($s0))
end

macro task1a(input_file)
    expr = :(+())
    for line in eachline(input_file)
        push!(expr.args, :(@dig2a($line)))
    end
    return esc(expr)
end

@task1a "test_input2.txt"

@task1a "input.txt"
