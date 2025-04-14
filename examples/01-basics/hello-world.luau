-- Module: example
-- This is the compiled output from hello-world.rkt

-- Define a function that creates a greeting
local function greet(name: string): string
  return "Hello, " .. name .. "!"
end

-- Call the function with different arguments
print(greet("World"))
print(greet("Roblox"))