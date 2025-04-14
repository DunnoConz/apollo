-- Module: example
-- This is the compiled output from basic-quasiquote.rkt

-- Process different message types using pattern matching
local function process_message(msg)
  if type(msg) == "table" then
    -- Simple greeting message with a name
    if msg[1] == "greet" and #msg == 2 then
      local name = msg[2]
      return string.format("Hello, %s!", name)
    
    -- Message with multiple parameters
    elseif msg[1] == "add" and #msg == 3 then
      local a = msg[2]
      local b = msg[3]
      return string.format("%s + %s = %s", tostring(a), tostring(b), tostring(a + b))
    
    -- Messages with nested structure
    elseif msg[1] == "person" and #msg == 3 and 
           type(msg[2]) == "table" and msg[2][1] == "name" and #msg[2] == 3 and
           type(msg[3]) == "table" and msg[3][1] == "age" and #msg[3] == 2 then
      local first = msg[2][2]
      local last = msg[2][3]
      local age = msg[3][2]
      return string.format("%s %s is %s years old", first, last, age)
    
    -- Messages with optional parts
    elseif msg[1] == "location" and #msg == 3 and type(msg[2]) == "table" and #msg[2] == 2 then
      local x = msg[2][1]
      local y = msg[2][2]
      local description = msg[3]
      return string.format("Location (%s, %s): %s", tostring(x), tostring(y), description)
    
    -- Messages with literal values
    elseif msg[1] == "status" and #msg == 3 and msg[2] == "online" then
      local username = msg[3]
      return string.format("%s is online", username)
    end
  end
  
  -- Default case
  return "Unknown message format"
end

-- Test the function with different message formats
local messages = {
  {"greet", "Alice"},
  {"add", 5, 3},
  {"person", {"name", "Bob", "Smith"}, {"age", 30}},
  {"location", {10, 20}, "Secret Hideout"},
  {"status", "online", "Charlie"},
  {"unknown", "format"}
}

-- Process each message
for _, msg in ipairs(messages) do
  print(string.format("Message: %s\nResponse: %s\n", tostring(msg), process_message(msg)))
end