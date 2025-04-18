#lang racket

;; Basic Quasiquote Patterns Example
;; Demonstrates using quasiquote in pattern matching

;; Process different message types using quasiquote pattern matching
(define (process-message msg)
  (match msg
    ;; Simple greeting message with a name
    [`(greet ,name)
     (format "Hello, ~a!" name)]
    
    ;; Message with multiple parameters
    [`(add ,a ,b)
     (format "~a + ~a = ~a" a b (+ a b))]
    
    ;; Messages with nested structure
    [`(person (name ,first ,last) (age ,age))
     (format "~a ~a is ~a years old" first last age)]
    
    ;; Messages with optional parts
    [`(location (,x ,y) ,description)
     (format "Location (~a, ~a): ~a" x y description)]
    
    ;; Messages with literal values
    [`(status "online" ,username)
     (format "~a is online" username)]
    
    ;; Default case
    [_ "Unknown message format"]))

;; Test the function with different message formats
(define messages
  (list '(greet "Alice")
        '(add 5 3)
        '(person (name "Bob" "Smith") (age 30))
        '(location (10 20) "Secret Hideout")
        '(status "online" "Charlie")
        '(unknown "format")))

(for ([msg messages])
  (printf "Message: ~s\nResponse: ~a\n\n" msg (process-message msg)))

;; Expected Luau output:
#|
local function process_message(msg)
  if msg[1] == "greet" and #msg == 2 then
    local name = msg[2]
    return string.format("Hello, %s!", name)
  elseif msg[1] == "add" and #msg == 3 then
    local a = msg[2]
    local b = msg[3]
    return string.format("%s + %s = %s", tostring(a), tostring(b), tostring(a + b))
  elseif msg[1] == "person" and #msg == 3 and msg[2][1] == "name" and #msg[2] == 3 and msg[3][1] == "age" and #msg[3] == 2 then
    local first = msg[2][2]
    local last = msg[2][3]
    local age = msg[3][2]
    return string.format("%s %s is %s years old", first, last, age)
  elseif msg[1] == "location" and #msg == 3 and #msg[2] == 2 then
    local x = msg[2][1]
    local y = msg[2][2]
    local description = msg[3]
    return string.format("Location (%s, %s): %s", tostring(x), tostring(y), description)
  elseif msg[1] == "status" and #msg == 3 and msg[2] == "online" then
    local username = msg[3]
    return string.format("%s is online", username)
  else
    return "Unknown message format"
  end
end

local messages = {
  {"greet", "Alice"},
  {"add", 5, 3},
  {"person", {"name", "Bob", "Smith"}, {"age", 30}},
  {"location", {10, 20}, "Secret Hideout"},
  {"status", "online", "Charlie"},
  {"unknown", "format"}
}

for _, msg in ipairs(messages) do
  print(string.format("Message: %s\nResponse: %s\n", tostring(msg), process_message(msg)))
end
|# 