-- Framework and assignment made by Jordy Perlee
-- Report any critical bugs to Jordy.Perlee@uva.nl

function love.conf(t)
    -- Global variables. Use these in your program.
    CELL_SIZE = 40
    NUM_ROWS = 15
    NUM_COLS = 10
    STATS_HEIGHT = 100

    -- Configuration settings for the game.
    t.window.height = CELL_SIZE * NUM_ROWS + STATS_HEIGHT
    t.window.width = CELL_SIZE * NUM_COLS
    t.window.title = "MineSweeper"
    t.window.resizable = false
end
