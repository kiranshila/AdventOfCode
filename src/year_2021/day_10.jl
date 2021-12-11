using DataStructures

input = readlines("resources/2021/10/input")

opens = Set(['(', '[', '{', '<'])
close = Set([')', ']', '}', '>'])
pairs = Dict(')' => '(', ']' => '[', '}' => '{', '>' => '<', '(' => ')', '[' => ']',
             '{' => '}', '<' => '>')
points = Dict(')' => 3, ']' => 57, '}' => 1197, '>' => 25137)

function process_line(line)
    s = Stack{Char}()
    for char in line
        if char ∈ opens
            push!(s, char)
        elseif char ∈ close
            top = first(s)
            if top != pairs[char]
                return points[char]
            else
                pop!(s)
            end
        end
    end
    return s
end

function part_1(input)
    errors = [process_line(line) for line in input]
    return sum(error for error in errors if error isa Real)
end

part_1(input)

point_mult = Dict(')' => 1, ']' => 2, '}' => 3, '>' => 4)

function score_incomplete(stack)
    score = 0
    while !isempty(stack)
        top = pop!(stack)
        closing = pairs[top]
        score *= 5
        score += point_mult[closing]
    end
    return score
end

function part_2(input)
    incomplete = [process_line(line) for line in input]
    filter!(x -> x isa Stack, incomplete)
    scores = [score_incomplete(stack) for stack in incomplete]
    sort!(scores)
    return scores[length(scores) ÷ 2 + 1]
end
