input = readlines("resources/2022/20/input") .|> x -> parse(Int, x)
test_input = [1, 2, -3, 3, -2, 0, 4]

wrap_index(i, n) = mod(i - 1, n - 1) + 1

function mix(file)
    N = length(file)
    print_order = 1:N |> collect
    current_idx = 1:N |> collect
    for (i, val) ∈ enumerate(file)
        idx = idxs[i]
        val = file[idxs][idx]
        new_idx_raw = val + idx
        new_idx = wrap_index(new_idx_raw, N)
        fix_range = (idx+1:new_idx)
        if fix_range[end] >= fix_range[1]
            idxs[(x->in(x, fix_range)).(idxs)] .-= 1
        else
            idxs[(x->in(x, fix_range)).(idxs)] .+= 1
        end
        idxs[i] = new_idx
    end
    file[idxs]
end