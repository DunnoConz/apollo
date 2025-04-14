-- Module: example
-- This is the compiled output from arithmetic.rkt

-- Basic arithmetic functions
local function add(a, b)
  return a + b
end

local function subtract(a, b)
  return a - b
end

local function multiply(a, b)
  return a * b
end

local function divide(a, b)
  return a / b
end

-- A more complex calculation
local function calculate_rectangle_properties(width, height)
  local area = width * height
  local perimeter = 2 * (width + height)
  -- Use parentheses to clarify order of operations
  local diagonal = math.sqrt((width * width) + (height * height))
  return {area, perimeter, diagonal}
end

-- Test the functions
print("Addition: " .. tostring(add(5, 3)))
print("Subtraction: " .. tostring(subtract(10, 4)))
print("Multiplication: " .. tostring(multiply(6, 7)))
print("Division: " .. tostring(divide(20, 5)))
print("Rectangle properties (width=5, height=3): ")
local properties = calculate_rectangle_properties(5, 3)
print("Area: " .. tostring(properties[1]) .. 
      ", Perimeter: " .. tostring(properties[2]) .. 
      ", Diagonal: " .. tostring(properties[3])) 