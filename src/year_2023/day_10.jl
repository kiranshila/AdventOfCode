using Graphs, SparseArrays
using GLMakie, GraphMakie
using GraphMakie.NetworkLayout

using GraphIO
using ParserCombinator
using CodecZlib
using LinearAlgebra

const EXAMPLE = open("resources/2023/10/example") do f
    read(f, String)
end

const INPUT = open("resources/2023/10/input") do f
    read(f, String)
end

const ΔS = Dict(
    'N' => (-1, 0),
    'S' => (1, 0),
    'E' => (0, 1),
    'W' => (0, -1),
)

const VALID = Dict(
    'N' => Set(['F', '7', '|']),
    'S' => Set(['L', 'J', '|']),
    'E' => Set(['J', '7', '-']),
    'W' => Set(['F', 'L', '-']),
)

const PIPE = Dict(
    '|' => Set(['N', 'S']),
    '-' => Set(['E', 'W']),
    'L' => Set(['N', 'E']),
    'J' => Set(['N', 'W']),
    '7' => Set(['S', 'W']),
    'F' => Set(['S', 'E']),
)

function build_graph(input)
    mat = reduce(vcat, permutedims.(collect.(eachline(IOBuffer(input)))))
    adj_size = prod(size(mat))
    g = spzeros(Bool, adj_size, adj_size)
    idx = coord -> (coord[1] - 1) * size(mat)[2] + coord[2]
    start_idx = findfirst(x -> x == 'S', mat)
    s_conn = []
    s_dir = Set{Char}()
    for (dir, Δ) ∈ ΔS
        di = Δ .+ Tuple(start_idx)
        if get(mat, di, '.') ∈ VALID[dir]
            push!(s_conn, di)
            push!(s_dir, dir)
        end
    end
    for (pipe, connections) ∈ PIPE
        if connections == s_dir
            mat[start_idx] = pipe
            break
        end
    end
    for (i, j) ∈ Tuple.(CartesianIndices(mat))
        c = mat[i, j]
        if c ∈ keys(PIPE)
            (s_dir, d_dir) = PIPE[c]
            s_idx = ΔS[s_dir] .+ (i, j)
            d_idx = ΔS[d_dir] .+ (i, j)
            if get(mat, s_idx, '.') ∈ VALID[s_dir] && get(mat, d_idx, '.') ∈ VALID[d_dir]
                g[idx(s_idx), idx(d_idx)] = true
                g[idx(d_idx), idx(s_idx)] = true
            end
        end
    end
    Graph(g), idx(start_idx), size(mat)
end

function part_one(input)
    g, s, _ = build_graph(input)
    components = filter(x -> s ∈ x, connected_components(g))[1]
    subg, vmap = induced_subgraph(g, components)
    s = findfirst(x -> x == s, vmap)
    ne(subg)
end

shoelacearea(x, y) =
    abs(sum(i * j for (i, j) in zip(x, append!(y[2:end], y[1]))) -
        sum(i * j for (i, j) in zip(append!(x[2:end], x[1]), y))) / 2

function part_two(input)
    g, s, si = build_graph(input)
    components = filter(x -> s ∈ x, connected_components(g))[1]
    subg, vmap = induced_subgraph(g, components)
    s = findfirst(x -> x == s, vmap)

    # Convex hull by walking graph
    cycle = cycle_basis(subg, s)[1]
    xs = []
    ys = []
    for v in cycle
        idx = vmap[v]
        i = (idx - 1) ÷ si[1] + 1
        j = (idx - 1) % si[2] + 1
        push!(xs, i)
        push!(ys, j)
    end
    #shape
    shoelacearea(xs, ys) / 4
end

# 941 - too high