#lang racket

;; Conditionals and control flow example
;; Demonstrates if, cond, and logical operations

;; Function to determine the grade based on a score
(define (grade-score score)
  (cond
    [(>= score 90) "A"]
    [(>= score 80) "B"] 
    [(>= score 70) "C"]
    [(>= score 60) "D"]
    [else "F"]))

;; Function to check if a number is even
(define (is-even? n)
  (if (= (remainder n 2) 0)
      #true
      #false))

;; A function showing nested conditions
(define (describe-number n)
  (let ([sign (cond
                [(> n 0) "positive"]
                [(< n 0) "negative"]
                [else "zero"])]
        [parity (if (is-even? n) "even" "odd")])
    (if (= n 0)
        "zero"
        (string-append 
          (if (= n 0) "" sign)
          (if (= n 0) "" " ")
          (if (= n 0) "" parity)))))

;; Test the functions with various inputs
(define scores '(95 85 75 65 55))
(for ([score scores])
  (printf "Score ~a: Grade ~a\n" score (grade-score score)))

(define numbers '(-4 -3 -2 -1 0 1 2 3 4))
(for ([number numbers])
  (printf "~a is ~a\n" number (describe-number number)))

;; Expected Luau output:
#|
local function grade_score(score)
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

local function is_even(n)
  if n % 2 == 0 then
    return true
  else
    return false
  end
end

local function describe_number(n)
  local sign
  if n > 0 then
    sign = "positive"
  elseif n < 0 then
    sign = "negative"
  else
    sign = "zero"
  end
  
  local parity
  if is_even(n) then
    parity = "even"
  else
    parity = "odd"
  end
  
  if n == 0 then
    return "zero"
  else
    local result = ""
    if n ~= 0 then
      result = result .. sign
    end
    if n ~= 0 then
      result = result .. " "
    end
    if n ~= 0 then
      result = result .. parity
    end
    return result
  end
end

local scores = {95, 85, 75, 65, 55}
for _, score in ipairs(scores) do
  print(string.format("Score %d: Grade %s", score, grade_score(score)))
end

local numbers = {-4, -3, -2, -1, 0, 1, 2, 3, 4}
for _, number in ipairs(numbers) do
  print(string.format("%d is %s", number, describe_number(number)))
end
|# 