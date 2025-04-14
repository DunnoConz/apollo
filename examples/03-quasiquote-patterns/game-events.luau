-- Module: example
-- This is a properly formatted Luau version of game-events.rkt

-- Define game state structure
local function create_game_state(player_position, health, score, inventory)
  return {
    player_position = player_position, -- {x, y} position
    health = health,                   -- 0-100 value
    score = score,                     -- numeric score
    inventory = inventory or {}        -- items in inventory
  }
end

-- Process different game events using pattern matching
local function process_event(state, event)
  -- Return a copy of state with specific fields updated
  local function state_copy(state, updates)
    local new_state = {}
    for k, v in pairs(state) do
      new_state[k] = v
    end
    for k, v in pairs(updates) do
      new_state[k] = v
    end
    return new_state
  end

  if event[1] == "move-player" and #event == 3 then
    local dx = event[2]
    local dy = event[3]
    local current_pos = state.player_position
    local new_x = current_pos[1] + dx
    local new_y = current_pos[2] + dy
    -- Keep position within bounds (0-100)
    local bounded_x = math.max(0, math.min(100, new_x))
    local bounded_y = math.max(0, math.min(100, new_y))
    
    return state_copy(state, {
      player_position = {bounded_x, bounded_y}
    })
    
  elseif event[1] == "damage" and #event == 2 then
    local amount = tonumber(event[2]) or 0
    local current_health = tonumber(state.health) or 0
    local new_health = math.max(0, current_health - amount)
    
    return state_copy(state, {
      health = new_health
    })
    
  elseif event[1] == "heal" and #event == 2 then
    local amount = tonumber(event[2]) or 0
    local current_health = tonumber(state.health) or 0
    local new_health = math.min(100, current_health + amount)
    
    return state_copy(state, {
      health = new_health
    })
    
  elseif event[1] == "add-score" and #event == 2 then
    local points = event[2]
    local current_score = state.score
    local new_score = current_score + points
    
    return state_copy(state, {
      score = new_score
    })
    
  elseif event[1] == "add-item" and #event == 3 then
    local item_id = event[2]
    local item_name = event[3]
    local current_inventory = state.inventory
    -- Create a new inventory table (clone it manually)
    local new_inventory = {}
    for i, item in ipairs(current_inventory) do
      new_inventory[i] = item
    end
    table.insert(new_inventory, {id = item_id, name = item_name})
    
    return state_copy(state, {
      inventory = new_inventory
    })
    
  elseif event[1] == "remove-item" and #event == 2 then
    local item_id = event[2]
    local current_inventory = state.inventory
    local new_inventory = {}
    
    for _, item in ipairs(current_inventory) do
      if item.id ~= item_id then
        table.insert(new_inventory, item)
      end
    end
    
    return state_copy(state, {
      inventory = new_inventory
    })
    
  elseif event[1] == "use-item" and #event == 3 and type(event[3]) == "table" then
    local item_id = event[2]
    local effect = event[3]
    
    if effect[1] ~= "effect" or #effect ~= 3 then
      return state
    end
    
    local effect_type = effect[2]
    local effect_value = effect[3]
    
    -- First remove the item from inventory
    local current_inventory = state.inventory
    local new_inventory = {}
    
    for _, item in ipairs(current_inventory) do
      if item.id ~= item_id then
        table.insert(new_inventory, item)
      end
    end
    
    local updated_state = state_copy(state, {
      inventory = new_inventory
    })
    
    -- Then apply the effect
    if effect_type == "health" then
      return process_event(updated_state, {"heal", effect_value})
    elseif effect_type == "score" then
      return process_event(updated_state, {"add-score", effect_value})
    else
      return updated_state
    end
  end
  
  -- Default: unknown event, return state unchanged
  return state
end

-- Create initial game state
local function create_initial_state()
  return create_game_state({50, 50}, 100, 0, {})
end

-- Run simulation with a series of events
local function run_game_simulation()
  local initial_state = create_initial_state()
  
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
  
  -- Process each event and update state
  local final_state = initial_state
  for _, event in ipairs(events) do
    local new_state = process_event(final_state, event)
    print(string.format("Event: %s", tostring(event)))
    print(string.format("New state: health=%d, score=%d, position={%d,%d}, inventory_size=%d\n", 
                       new_state.health, 
                       new_state.score, 
                       new_state.player_position[1], 
                       new_state.player_position[2],
                       #new_state.inventory))
    final_state = new_state
  end
  
  -- Print final state summary
  print("Final state after all events:")
  print(string.format("Position: (%d, %d)", 
                     final_state.player_position[1],
                     final_state.player_position[2]))
  print(string.format("Health: %d/100", final_state.health))
  print(string.format("Score: %d", final_state.score))
  print(string.format("Inventory items: %d", #final_state.inventory))
  
  return final_state
end

-- Run the simulation
run_game_simulation()