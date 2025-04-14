--!strict
-- This is the compiled output from conditionals.rkt

-- Function to determine the grade based on a score
local function gradeScore(score: number): string
    if score >= 90 then
        return "A"
    elseif score >= 80 then
        return "B"
    elseif score >= 70 then
        return "C"
    elseif score >= 60 then
        return "D"
    else
        return "F"
    end
end

-- Function to check if a number is even
local function isEven(n: number): boolean
    return n % 2 == 0
end

-- A function showing nested conditions
local function describeNumber(n: number): string
    if n == 0 then
        return "zero"
    end
    
    local sign = if n > 0 
        then "positive" 
        elseif n < 0 
        then "negative" 
        else "zero"
    
    local parity = if isEven(n) 
        then "even" 
        else "odd"
    
    return sign .. " " .. parity
end

-- Test the functions with various inputs
local scores = {95, 85, 75, 65, 55}
for _, score in ipairs(scores) do
    print(string.format("Score %d: Grade %s", score, gradeScore(score)))
end

local numbers = {-4, -3, -2, -1, 0, 1, 2, 3, 4}
for _, number in ipairs(numbers) do
    print(string.format("%d is %s", number, describeNumber(number)))
end 