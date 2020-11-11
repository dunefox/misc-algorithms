# 8 queens problem

# Positions
positions = []

# Make 8x8 array
board1 = [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1  0  0  0  0  0  0  0  0 -1;
          -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]

board2 = zeros(Int64, 8, 8)

diag1(a, b) = abs(a[1] - b[1]) != abs(a[2] - b[2])
diag2(a, b) = a[1] + a[2] != b[1] + b[2]
row(a, b) = a[1] != b[1]
col(a, b) = a[2] != b[2]

function check_pos(qpos)
    ldiag = all((x -> diag1(qpos, x)).(positions))
    rdiag = all((x -> diag2(qpos, x)).(positions))
    colp = all((x -> col(qpos, x)).(positions))
    return ldiag && rdiag && colp
end

function place_at((x, y))
    @info "Queen placed" x y
    @assert (x, y) ∉ positions "Queen already in place"
    push!(positions, (x, y))
end

function main()
    for i in 1:8
        for j in 1:8
            if check_pos((i, j))
                place_at((i, j))
                break
            end
        end
    end
end

main()

# Run tests
# board2[1, 1] = 1
# @info check_pos((1, 1)) check_pos((2, 2)) check_pos((2, 1))

# Create and print board
for (i, j) in positions
    board2[i, j] = 1
end

println(positions)
show(stdout, "text/plain", board2)
