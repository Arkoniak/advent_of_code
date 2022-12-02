SCORE = [0 1 -1; -1 0 1; 1 -1 0]
score(i, j) = (SCORE[i, j] == 0 ? 3 : SCORE[i, j] == 1 ? 6 : 0) + j

function c2id(c)
    if c == 'A'
        return 1
    elseif c == 'B'
        return 2
    elseif c == 'C'
        return 3
    elseif c == 'X'
        return 1
    elseif c == 'Y'
        return 2
    else
        return 3
    end
end

function c2id(id1, c)
    v = c == 'X' ? -1 : c == 'Y' ? 0 : 1
    findfirst(==(v), SCORE[id1, :])
end

# Part 1
sum(row -> score(c2id(row[1]), c2id(row[3])), eachline("input.txt"))

# Part 2
sum(row -> score(c2id(row[1]), c2id(c2id(row[1]), row[3])), eachline("input.txt"))
