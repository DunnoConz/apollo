#lang racket

;; Game Events with Quasiquote Pattern Matching
;; Demonstrates how to process game events using quasiquote patterns

;; Define game state
(struct game-state (player-position health score inventory) #:transparent)

;; Process game events and update state
(define (process-event state event)
  (match event
    ;; Player movement events
    [`(move-player ,dx ,dy)
     (let* ([current-pos (game-state-player-position state)]
            [new-x (+ (car current-pos) dx)]
            [new-y (+ (cdr current-pos) dy)]
            ;; Ensure player stays within bounds (0-100)
            [bounded-x (max 0 (min 100 new-x))]
            [bounded-y (max 0 (min 100 new-y))]
            [new-pos (cons bounded-x bounded-y)])
       (struct-copy game-state state
                   [player-position new-pos]))]
    
    ;; Health-related events
    [`(damage ,amount)
     (let* ([current-health (game-state-health state)]
            [new-health (max 0 (- current-health amount))])
       (struct-copy game-state state
                   [health new-health]))]
    
    [`(heal ,amount)
     (let* ([current-health (game-state-health state)]
            [new-health (min 100 (+ current-health amount))])
       (struct-copy game-state state
                   [health new-health]))]
    
    ;; Score events
    [`(add-score ,points)
     (let* ([current-score (game-state-score state)]
            [new-score (+ current-score points)])
       (struct-copy game-state state
                   [score new-score]))]
    
    ;; Inventory events
    [`(add-item ,item-id ,item-name)
     (let* ([current-inventory (game-state-inventory state)]
            [new-item (cons item-id item-name)]
            [new-inventory (cons new-item current-inventory)])
       (struct-copy game-state state
                   [inventory new-inventory]))]
    
    [`(remove-item ,item-id)
     (let* ([current-inventory (game-state-inventory state)]
            [new-inventory (filter (lambda (item) 
                                    (not (equal? (car item) item-id)))
                                  current-inventory)])
       (struct-copy game-state state
                   [inventory new-inventory]))]
    
    ;; Complex events with nested patterns
    [`(use-item ,item-id (effect ,effect-type ,effect-value))
     (let* ([current-inventory (game-state-inventory state)]
            [new-inventory (filter (lambda (item) 
                                   (not (equal? (car item) item-id)))
                                 current-inventory)]
            ;; Apply effect based on type
            [updated-state (struct-copy game-state state
                                      [inventory new-inventory])])
       (match effect-type
         ['health (process-event updated-state `(heal ,effect-value))]
         ['score (process-event updated-state `(add-score ,effect-value))]
         [_ updated-state]))]
    
    ;; Default: return unchanged state
    [_ state]))

;; Create initial game state
(define (create-initial-state)
  (game-state (cons 50 50)  ; player at center of map
             100           ; full health
             0             ; starting score
             '()))         ; empty inventory

;; Test the game event system
(define (run-game-simulation)
  (let* ([initial-state (create-initial-state)]
         [events (list 
                  '(move-player 10 5)          ; Move player
                  '(add-item "potion" "Health Potion")
                  '(add-item "gem" "Score Gem")
                  '(damage 30)                 ; Take damage
                  '(use-item "potion" (effect health 20))  ; Use healing item
                  '(add-score 50)              ; Score points
                  '(use-item "gem" (effect score 100))     ; Use score item
                  '(move-player -20 15))])     ; Move again
    
    ;; Process events in sequence
    (let ([final-state (foldl (lambda (event state)
                              (let ([new-state (process-event state event)])
                                (printf "Event: ~s\n" event)
                                (printf "New state: ~s\n\n" new-state)
                                new-state))
                            initial-state
                            events)])
      
      ;; Print final summary
      (printf "Final state after all events:\n")
      (printf "Position: (~a, ~a)\n" 
              (car (game-state-player-position final-state))
              (cdr (game-state-player-position final-state)))
      (printf "Health: ~a/100\n" (game-state-health final-state))
      (printf "Score: ~a\n" (game-state-score final-state))
      (printf "Inventory items: ~a\n" 
              (length (game-state-inventory final-state))))))

;; Run the simulation
(run-game-simulation)

;; Expected Luau output:
#|
-- Game state definition
local function game_state(player_position, health, score, inventory)
  return {
    player_position = player_position,
    health = health,
    score = score,
    inventory = inventory
  }
end

-- Process game events
local function process_event(state, event)
  if event[1] == "move-player" and #event == 3 then
    local dx, dy = event[2], event[3]
    local current_pos = state.player_position
    local new_x = current_pos[1] + dx
    local new_y = current_pos[2] + dy
    -- Ensure player stays within bounds
    local bounded_x = math.max(0, math.min(100, new_x))
    local bounded_y = math.max(0, math.min(100, new_y))
    local new_pos = {bounded_x, bounded_y}
    return game_state(new_pos, state.health, state.score, state.inventory)
    
  elseif event[1] == "damage" and #event == 2 then
    local amount = event[2]
    local new_health = math.max(0, state.health - amount)
    return game_state(state.player_position, new_health, state.score, state.inventory)
    
  elseif event[1] == "heal" and #event == 2 then
    local amount = event[2]
    local new_health = math.min(100, state.health + amount)
    return game_state(state.player_position, new_health, state.score, state.inventory)
    
  elseif event[1] == "add-score" and #event == 2 then
    local points = event[2]
    local new_score = state.score + points
    return game_state(state.player_position, state.health, new_score, state.inventory)
    
  elseif event[1] == "add-item" and #event == 3 then
    local item_id, item_name = event[2], event[3]
    local new_item = {item_id, item_name}
    local new_inventory = {new_item}
    for _, item in ipairs(state.inventory) do
      table.insert(new_inventory, item)
    end
    return game_state(state.player_position, state.health, state.score, new_inventory)
    
  elseif event[1] == "remove-item" and #event == 2 then
    local item_id = event[2]
    local new_inventory = {}
    for _, item in ipairs(state.inventory) do
      if item[1] ~= item_id then
        table.insert(new_inventory, item)
      end
    end
    return game_state(state.player_position, state.health, state.score, new_inventory)
    
  elseif event[1] == "use-item" and #event == 3 and event[3][1] == "effect" and #event[3] == 3 then
    local item_id = event[2]
    local effect_type = event[3][2]
    local effect_value = event[3][3]
    
    -- Remove item from inventory
    local new_inventory = {}
    for _, item in ipairs(state.inventory) do
      if item[1] ~= item_id then
        table.insert(new_inventory, item)
      end
    end
    
    local updated_state = game_state(state.player_position, state.health, state.score, new_inventory)
    
    -- Apply effect
    if effect_type == "health" then
      return process_event(updated_state, {"heal", effect_value})
    elseif effect_type == "score" then
      return process_event(updated_state, {"add-score", effect_value})
    else
      return updated_state
    end
  
  else
    -- Default: return unchanged state
    return state
  end
end

-- Create initial game state
local function create_initial_state()
  return game_state({50, 50}, 100, 0, {})
end

-- Run simulation
local function run_game_simulation()
  local state = create_initial_state()
  local events = {
    {"move-player", 10, 5},
    {"add-item", "potion", "Health Potion"},
    {"add-item", "gem", "Score Gem"},
    {"damage", 30},
    {"use-item", "potion", {"effect", "health", 20}},
    {"add-score", 50},
    {"use-item", "gem", {"effect", "score", 100}},
    {"move-player", -20, 15}
  }
  
  -- Process events
  for _, event in ipairs(events) do
    state = process_event(state, event)
    print("Event: " .. tostring(event))
    print("New state: " .. tostring(state))
    print("")
  end
  
  -- Print summary
  print("Final state after all events:")
  print(string.format("Position: (%d, %d)", state.player_position[1], state.player_position[2]))
  print(string.format("Health: %d/100", state.health))
  print(string.format("Score: %d", state.score))
  print(string.format("Inventory items: %d", #state.inventory))
end

run_game_simulation()
|# 