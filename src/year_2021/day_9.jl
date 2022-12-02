using DelimitedFiles, LoopVectorization

parse_input(lines) = vcat([map(x -> parse(Int, x), collect(line))' for line in lines]...)

input = readlines("resources/2021/9/input")

A = parse_input(input)

function minima(A)
    m, n = size(A)
    mat = fill(9, m + 2, n + 2)
    mat[2:(end - 1), 2:(end - 1)] = A
    mins = CartesianIndex{2}[]
    @inbounds for j in 2:(n + 1), i in 2:(m + 1)
        here = mat[i, j]
        l = mat[i, j - 1]
        r = mat[i, j + 1]
        u = mat[i - 1, j]
        d = mat[i + 1, j]
        if (here < l) && (here < r) && (here < u) && (here < d)
            push!(mins, CartesianIndex(i - 1, j - 1))
        end
    end
    return mins
end

part_1(A) = sum(A[minima(A)] .+ 1)

function basin_size(mat, idx, visited=Set{CartesianIndex{2}}())
    here = mat[idx]
    l = idx + CartesianIndex(0, -1)
    r = idx + CartesianIndex(0, 1)
    u = idx + CartesianIndex(-1, 0)
    d = idx + CartesianIndex(1, 0)
    sl = (9 > mat[l] > here) && !(l ∈ visited) ? basin_size(mat, l, push!(visited, l)) : 0
    sr = (9 > mat[r] > here) && !(r ∈ visited) ? basin_size(mat, r, push!(visited, r)) : 0
    su = (9 > mat[u] > here) && !(u ∈ visited) ? basin_size(mat, u, push!(visited, u)) : 0
    sd = (9 > mat[d] > here) && !(d ∈ visited) ? basin_size(mat, d, push!(visited, d)) : 0
    return sl + sr + su + sd + 1
end


function part_2(A)
    m, n = size(A)
    mat = fill(9, m + 2, n + 2)
    mat[2:(end - 1), 2:(end - 1)] = A
    mins = minima(A)
    sizes = sort([basin_size(mat,idx + CartesianIndex(1,1)) for idx ∈ mins])[end-2:end]
    return *(sizes...)
end