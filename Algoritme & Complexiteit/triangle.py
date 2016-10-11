# source: http://rosettacode.org/wiki/Maximum_triangle_path_sum

# Find path with biggest sum in triangle:
def biggest_sum(triangle):
    while len(triangle) > 1:
        # Take 2 lowest rows
        t0 = triangle.pop()
        t1 = triangle.pop()

        # Take biggest of 2 values plus maximum of both paths
        max_i = [max(t0[i], t0[i+1]) + t for i, t in enumerate(t1)]
        triangle.append(max_i)
    return triangle[0][0]

# Read file and split lines in 1d array
data = open("triangle.txt", "r").readlines()

# Split 1d array into 2d array and map to int
for i, t in enumerate(data):
    data[i] = map(int, t.split())

print biggest_sum(data)
