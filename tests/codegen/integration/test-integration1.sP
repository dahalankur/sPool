# test-integration1.sP
# Demos sPool's threads by implementing a simple BFS algorithm
# Inspired by CS40's unblackededges assignment at Tufts University
# Written by Team Nautilus (ankur, etha, max, yuma)

def bool is_black(int row, int col, list<list<int>> grid):
    return List_at(List_at(grid, row), col) == 1
;

# simple function that simply prints the grid to stdout
def quack print_grid(list<list<int>> grid):
    int i = 0
    while (i < List_len(grid)):
        int j = 0
        while (j < List_len(List_at(grid, 0))):
            print(int_to_string(List_at(List_at(grid, i), j)))
            print(" ")
            j = j + 1
        ;
        println("")
        i = i + 1
    ;
    println("")
    return
;

# function that checks the neighbors of the current pixel at (row, col) in the grid and 
# adds them to the queue if they are black pixels
def quack check_neighbor(int row, int col, list<list<int>> grid, list<list<int>> queue):
    if (row < List_len(grid) - 1):             if (is_black(row + 1, col, grid)): List_insert(queue, List_len(queue), [row + 1, col]);;
    if (col < List_len(List_at(grid, 0)) - 1): if (is_black(row, col + 1, grid)): List_insert(queue, List_len(queue), [row, col + 1]);;
    if (row > 0):                              if (is_black(row - 1, col, grid)): List_insert(queue, List_len(queue), [row - 1, col]);;
    if (col > 0):                              if (is_black(row, col - 1, grid)): List_insert(queue, List_len(queue), [row, col - 1]);;
    return
;

list<list<int>> grid = [[1,1,1,1,1,1,1,1],[1,0,0,0,0,0,0,1],[1,0,0,1,1,0,0,1],[1,0,1,0,0,0,1,1],[1,0,1,1,1,0,1,1],[1,0,1,1,1,0,0,1],[1,0,0,0,0,0,1,0],[1,1,1,1,1,1,0,1]]
list<list<int>> queue = []

int row = 0
int rows = List_len(grid)
int cols = List_len(List_at(grid, 0))

# populate the initial queue with the black edges from the edge of the image
while (row < rows):
    int col = 0
    while (col < cols):
        if ((row == 0 || row == rows - 1 || col == 0 || col == cols - 1) && is_black(row, col, grid)):
            List_insert(queue, List_len(queue), [row, col])
        ;
        col = col + 1
    ;
    row = row + 1
;

println("BEFORE UNBLACKING: ")
print_grid(grid)

# BFS; go through the queue, replacing any black pixels with white and 
#      repeating for its neighbor if it is also a black pixel
while (List_len(queue) > 0):
    int row = List_at(List_at(queue, 0), 0)
    int col = List_at(List_at(queue, 0), 1)
    List_replace(List_at(grid, row), col, 0)
    check_neighbor(row, col, grid, queue) # queue is changed by reference if required by this function 
    List_remove(queue, 0)
;

println("AFTER UNBLACKING: ")
print_grid(grid)