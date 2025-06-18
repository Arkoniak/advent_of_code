using PikaParser
const P = PikaParser

s = collect(eachline("test_input.txt"))[1]

rules = Dict(
             :digit => P.satisfy(isdigit),
             :letter => P.satisfy(isletter),
             :digits => P.some(:digit),
             :word => P.some(:letter),
             :ws => P.many(P.first(P.satisfy(isspace), P.token(','), P.token(';'))),
             :cube => P.seq(:digits, :ws, :word, :ws),
             :game => P.seq(P.token('G'), P.token('a'), P.token('m'), P.token('e')),
             :expr => P.seq(:game, :ws, :digits, P.token(':'), :ws, :line => P.some(:cube))
            )

g = P.make_grammar(
    [:expr], # the top-level rule
    P.flatten(rules, Char), # process the rules into a single level and specialize them for crunching Chars
)

p = P.parse(g, s)

p.matches[2]

function fold_scheme(m, p, s)
    m.rule == :digits ? parse(Int, m.view) :
    m.rule == :word ? Symbol(m.view) : 
    m.rule == :cube ? Expr(:call, s[3], s[1]) :
    m.rule == :line ? s :
    m.rule == :expr ? Expr(:call, :game, s[3], s[6]...) : nothing
end

game1(s...) = (&)(s...)

game1(true, true, false)

macro game(line)
    p = P.parse(g, line)
    esc(P.traverse_match(p, P.find_match_at!(p, :expr, 1), fold = fold_scheme))
end

macro day2(file)
    expr = :(+())
    for line in eachline(file)
        push!(expr.args, :(@game $line))
    end
    return esc(expr)
end

blue(n) = n <= 14
green(n) = n <= 13
red(n) = n <= 12

game(id, tests...) = all(tests) ? id : 0

@macroexpand @day2 "test_input.txt"
@day2 "test_input.txt"
# 2169
@day2 "input.txt"

macro game(line)
    p = P.parse(g, line)
    r = esc(P.traverse_match(p, P.find_match_at!(p, :expr, 1), fold = fold_scheme))
    
    args = r.args[1].args[3:end]
    expr = :(let blue = 0, green = 0, red = 0 end)
    for el in args
        func = el.args[1]
        push!(expr.args[2].args, :($func = max($func, $(el.args[2]))))
    end
    push!(expr.args[2].args, :(blue*red*green))

    return esc(expr)
end

expr = :(let blue1 = 0, green1 = 0 end)
push!(expr.args[2].args, :(println(blue1 + 1)))
eval(expr)
dump(:(let blue1 = 0, green1 = 0 end))

@macroexpand @game "Game 1: 2 blue, 3 red, 4 green"
@game "Game 1: 2 blue, 3 red, 4 green"

@macroexpand @day2 "test_input.txt"
@day2 "test_input.txt"
@day2 "input.txt"

    # fold_scheme(m, p, s) = (println(m.rule), String(m.view))
#
dump(P.traverse_match(p, P.find_match_at!(p, :expr, 1), fold = fold_scheme))




eval(P.traverse_match(p, P.find_match_at!(p, :expr, 1), fold = fold_scheme))


dump(P.traverse_match(p, P.find_match_at!(p, :expr, 1)))

