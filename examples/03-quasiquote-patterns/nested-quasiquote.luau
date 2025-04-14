-- Module: example
-- This is the compiled output from nested-quasiquote.rkt

-- Process database query-like structures using pattern matching
local function process_query(query)
  if query[1] == "select" and #query >= 2 and query[2][1] == "fields" and query[3][1] == "from" then
    -- Extract fields
    local fields = {}
    for i = 2, #query[2] do
      table.insert(fields, query[2][i])
    end
    local table_name = query[3][2]
    
    -- Simple select
    if #query == 3 then
      -- SELECT fields FROM table
      return string.format("SELECT %s FROM %s", 
                          table.concat(fields, ", "), 
                          table_name)
                          
    -- Select with where clause
    elseif #query == 4 and query[4][1] == "where" and query[4][2][1] == "=" then
      local field = query[4][2][2]
      local value = query[4][2][3]
      return string.format("SELECT %s FROM %s WHERE %s = %s", 
                          table.concat(fields, ", "), 
                          table_name,
                          field,
                          type(value) == "string" and string.format("%q", value) or tostring(value))
                          
    -- Nested queries with joins
    elseif #query == 4 and query[4][1] == "join" then
      local join_type = query[4][2]
      local join_table = query[4][3]
      local on_clause = query[4][4]
      local table_field = on_clause[2][2]
      local join_field = on_clause[2][3]
      
      return string.format("SELECT %s FROM %s %s JOIN %s ON %s.%s = %s.%s", 
                          table.concat(fields, ", "), 
                          table_name,
                          join_type,
                          join_table,
                          table_name,
                          table_field,
                          join_table,
                          join_field)
    end
    
  -- Aggregate query
  elseif query[1] == "select" and #query >= 3 and 
         query[2][1] == "aggregate" and 
         query[3][1] == "from" and 
         query[4][1] == "group-by" then
    
    local agg_type = query[2][2]
    local agg_field = query[2][3]
    local table_name = query[3][2]
    local group_fields = {}
    for i = 2, #query[4] do
      table.insert(group_fields, query[4][i])
    end
    
    return string.format("SELECT %s(%s) FROM %s GROUP BY %s", 
                        agg_type,
                        agg_field,
                        table_name,
                        table.concat(group_fields, ", "))
                        
  -- Insert query
  elseif query[1] == "insert" and #query == 3 and 
         query[2][1] == "into" and 
         query[3][1] == "values" then
    
    local table_name = query[2][2]
    local fields = {}
    local values = {}
    
    for i = 2, #query[3] do
      local pair = query[3][i]
      if type(pair) == "table" and #pair == 2 then
        table.insert(fields, pair[1])
        table.insert(values, type(pair[2]) == "string" and 
                           string.format("%q", pair[2]) or 
                           tostring(pair[2]))
      end
    end
    
    return string.format("INSERT INTO %s (%s) VALUES (%s)", 
                        table_name,
                        table.concat(fields, ", "),
                        table.concat(values, ", "))
                        
  -- Update query (specific structure for update statements)
  elseif type(query) == "table" and query[1] == "update" and #query == 4 then
    -- Get the table name (should be a string)
    local table_name = query[2]
    if type(table_name) ~= "string" then
      return "INVALID QUERY: Table name must be a string"
    end
    
    -- Get the set clause (should be a table with structure {"set", {{"field1", "value1"}, ...}})
    local set_clause = query[3]
    if type(set_clause) ~= "table" or set_clause[1] ~= "set" then
      return "INVALID QUERY: Missing or invalid SET clause"
    end
    
    -- Get the field-value pairs from the set clause
    local set_pairs = {}
    for i = 2, #set_clause do
      local pair = set_clause[i]
      if type(pair) == "table" and #pair == 2 then
        local field = pair[1]
        local value = pair[2]
        -- Format the value based on its type
        local formatted_value = type(value) == "string" and 
                                  string.format("%q", value) or 
                                  tostring(value)
        -- Add to the set pairs as a string
        table.insert(set_pairs, tostring(field) .. " = " .. tostring(formatted_value))
      end
    end
    
    if #set_pairs == 0 then
      return "INVALID QUERY: No valid SET pairs found"
    end
    
    -- Debug set_pairs before concatenation
    print("Debug set_pairs:")
    for i, v in ipairs(set_pairs) do
        print("  " .. i .. ": " .. tostring(v) .. " (type: " .. type(v) .. ")")
    end
    
    -- Get the where clause (should be a table with structure {"where", {"=", "field", value}})
    local where_clause = query[4]
    if type(where_clause) ~= "table" or where_clause[1] ~= "where" then
      return "INVALID QUERY: Missing or invalid WHERE clause"
    end
    
    -- Get the condition from the where clause
    local condition = where_clause[2]
    if type(condition) ~= "table" or condition[1] ~= "=" or #condition ~= 3 then
      return "INVALID QUERY: Invalid WHERE condition"
    end
    
    local where_field = condition[2]
    local where_value = condition[3]
    local formatted_where_value = type(where_value) == "string" and 
                                   string.format("%q", where_value) or 
                                   tostring(where_value)
    
    -- Build the final SQL statement
    return string.format("UPDATE %s SET %s WHERE %s = %s", 
                        table_name,
                        table.concat(set_pairs, ", "),
                        where_field,
                        formatted_where_value)
                        
  -- Delete query
  elseif query[1] == "delete" and #query == 3 and 
         query[2][1] == "from" then
    
    local table_name = query[2][2]
    local condition = query[3][2]
    
    local where_clause = "true"
    if type(condition) == "table" and condition[1] == "=" then
      where_clause = string.format("%s = %s", 
                                condition[2], 
                                type(condition[3]) == "string" and 
                                  string.format("%q", condition[3]) or 
                                  tostring(condition[3]))
    end
    
    return string.format("DELETE FROM %s WHERE %s", 
                        table_name,
                        where_clause)
  end
  
  -- Default - unknown query type
  return "INVALID QUERY"
end

-- Test the query processor with various queries
local queries = {
  -- Simple select query
  {"select", {"fields", "id", "name", "age"}, {"from", "users"}},
  
  -- Select with where clause
  {"select", {"fields", "id", "name"}, 
            {"from", "users"}, 
            {"where", {"=", "status", "active"}}},
  
  -- Nested query with join
  {"select", {"fields", "users.id", "orders.id", "orders.amount"}, 
            {"from", "users"}, 
            {"join", "inner", "orders", {"on", {"=", "id", "user_id"}}}},
  
  -- Aggregate query
  {"select", {"aggregate", "sum", "amount"}, {"from", "orders"}, {"group-by", "user_id", "status"}},
  
  -- Insert query
  {"insert", {"into", "users"}, 
            {"values", {"name", "John Doe"}, {"age", 30}, {"email", "john@example.com"}}},
  
  -- Update query
  {"update", "users", 
            {"set", {{"name", "Jane Doe"}, {"age", 32}}}, 
            {"where", {"=", "id", 123}}},
  
  -- Delete query
  {"delete", {"from", "users"}, {"where", {"=", "status", "inactive"}}}
}

-- Process each query and display the result
for _, query in ipairs(queries) do
  local result = process_query(query)
  print(string.format("Query: %s", tostring(query)))
  
  -- Ensure result is a string before using it in string.format
  if type(result) == "string" then
    print(string.format("SQL: %s\n", result))
  else
    print("SQL: [Error: Non-string result]")
  end
end