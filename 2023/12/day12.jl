function input_data(filename="test_input.txt")
    map(eachline(filename)) do line
        s0, v0 = split(line, " ")
        vals = parse.(Int, split(v0, ","))
        s0, vals
    end
end

data = input_data()
datum = data[1]

split(datum, " ")
