--  Name          :     Steven Raaijmakers
--  Student ID    :     10804242
--  Omschrijving  :     Programma simuleert het spel Mijnenveger
--  Bronnen       :     https://github.com/maker91/minesweeper/blob/master/main.lua (voor de timer)

-- Cell (object)
Cell = {
    x = 0;
    y = 0;
    number = 0;
    clicked = false;
    mine = false;
}

Cell.__index = Cell

-- Cell functie new
function Cell:new(i, j)
    local object = setmetatable({}, Cell)
    object.x = i
    object.y = j

    -- Random mijnen leggen (+-15% kans op mijn)
    local random = love.math.random(0, 100)
    if random <= 15 then
        object.mine = true
    end

    return object;
end

--  Cell functie waarbij gekeken wordt naar omliggende cellen
function Cell:checkNeighbors()
    if self.clicked == false then
        self.clicked = true

        -- Standaard minima's en maxima's
        local min_x = self.x - 1
        local max_x = self.x + 1

        local min_y = self.y - 1
        local max_y = self.y + 1

        -- Voorwaardes voor cellen met minder dan 8 omliggende cellen
        if self.x == 0 then
            min_x = 0
        elseif self.x == NUM_COLS - 1 then
            max_x = NUM_COLS - 1
        end
        if self.y == 0 then
            min_y = 0
        elseif self.y == NUM_ROWS - 1 then
            max_y = NUM_ROWS - 1
        end

        -- For loop omliggende cellen (print aantal mijnen)
        for x = min_x, max_x, 1 do
            for y = min_y, max_y, 1 do
                if Grid[x][y].mine == true then
                    self.number = self.number + 1
                end
            end
        end

        -- For loop opent omliggende lege cellen (recursie)
        for x = min_x, max_x, 1 do
            for y = min_y, max_y, 1 do
                if self.number == 0 then
                    Grid[x][y]:checkNeighbors()
                end
            end
        end
    end
end

--  Love.load functie
function love.load()
    -- Programma begin variabelen
    lose = false
    win = false
    time = 0
	timer_paused = true

    -- 2D array (Grid) vullen met Cell
    Grid = {}
    for x = 0, NUM_COLS, 1 do
        Grid[x] = {}
        for y = 0, NUM_ROWS, 1 do
            Grid[x][y] = Cell:new(x, y)
        end
    end

    -- Aantal cellen
    total = NUM_COLS * NUM_ROWS

    -- Tel aantal mijnen op speelveld
    mines = 0
    for x = 0, NUM_COLS-1, 1 do
        for y = 0, NUM_ROWS-1, 1 do
            if Grid[x][y].mine == true then
                mines = mines + 1
            end
        end
    end
end

-- Update functie (netter dan in draw!!)
function love.update(dt)
	if timer_paused == false then -- timer begint met tellen ná eerste klik
		time = time + dt
	end

    -- Telt alle "schone" cellen op het grid
    clean = 0
    for x = 0, NUM_COLS - 1, 1 do
        for y = 0, NUM_ROWS - 1, 1 do
            if Grid[x][y].mine == true and Grid[x][y].clicked == true then
                lose = true
                timer_paused = true
                -- Onruim veld
                for x = 0, NUM_COLS, 1 do
                    for y = 0, NUM_ROWS, 1 do
                        if Grid[x][y].mine == true then
                            Grid[x][y].clicked = true
                        end
                    end
                end
            end
            if Grid[x][y].clicked == false and Grid[x][y].mine == false then
                clean = clean + 1
            end
        end
    end

    if clean == 0 then
        win = true
        timer_paused = true
    end

end

--  Love.draw functie
function love.draw()

    -- Statsbalk
    love.graphics.setColor(255, 255, 255) -- wit
    love.graphics.print("Cells: " .. clean, 10, STATS_HEIGHT / 3)
    love.graphics.printf("Time: " .. math.floor(time) .. " s", 0, STATS_HEIGHT / 3, CELL_SIZE * NUM_COLS - 10, "right")

    -- Grid tekenen
    for x = 0, NUM_COLS, 1 do
        for y = 0, NUM_ROWS, 1 do
            love.graphics.setColor(255, 255, 255) -- wit
            love.graphics.rectangle("line", x * CELL_SIZE, y * CELL_SIZE + STATS_HEIGHT, CELL_SIZE, CELL_SIZE)

            if Grid[x][y].clicked == true then
                love.graphics.setColor(255, 255, 255) -- wit
                if Grid[x][y].mine == true then
                    love.graphics.setColor(255, 0, 0) -- rood
                end
                love.graphics.rectangle("fill", x * CELL_SIZE + 2, y * CELL_SIZE + 2 + STATS_HEIGHT, CELL_SIZE - 4, CELL_SIZE - 4)
                if Grid[x][y].number > 0 and Grid[x][y].mine == false then -- nummer printen
                    love.graphics.setColor(0, 0, 0) -- zwart
                    love.graphics.printf(Grid[x][y].number, x * CELL_SIZE, y * CELL_SIZE + (CELL_SIZE / 3) + STATS_HEIGHT, CELL_SIZE, "center")
                end
            end
        end
    end

    -- Print message in statsbalk
    if win == true then
        love.graphics.setColor(128, 255, 0) -- groen
        message = "You won!"
        love.graphics.printf(message, 0, STATS_HEIGHT / 3, CELL_SIZE * NUM_COLS, "center")
    elseif lose == true then
        love.graphics.setColor(255, 0, 0) -- rood
        message = "You lost!"
        love.graphics.printf(message, 0, STATS_HEIGHT / 3, CELL_SIZE * NUM_COLS, "center")
    end
end

--  Love.mousepressed functie
function love.mousepressed(x, y, button)

    if lose == false and win == false then
    	local mouse_x = math.floor(x / CELL_SIZE) -- x
    	local mouse_y = math.floor((y - STATS_HEIGHT) / CELL_SIZE) -- y

        -- Checks
        if button == "l" and y > STATS_HEIGHT then
            timer_paused = false
            Grid[mouse_x][mouse_y]:checkNeighbors()
        end
    end
end
