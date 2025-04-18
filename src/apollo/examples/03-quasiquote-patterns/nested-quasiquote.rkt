#lang racket

;; Nested Quasiquote Patterns Example
;; Demonstrates using quasiquote patterns with nesting and complex data

;; Process database query-like structures using quasiquote patterns
(define (process-query query)
  (match query
    ;; Simple select query
    [`(select (fields ,fields ...) (from ,table))
     (format "SELECT ~a FROM ~a" 
             (string-join (map symbol->string fields) ", ")
             (symbol->string table))]
    
    ;; Select with where clause
    [`(select (fields ,fields ...) (from ,table) (where (= ,field ,value)))
     (format "SELECT ~a FROM ~a WHERE ~a = ~s" 
             (string-join (map symbol->string fields) ", ")
             (symbol->string table)
             (symbol->string field)
             value)]
    
    ;; Nested queries with joins
    [`(select (fields ,fields ...) 
             (from ,table) 
             (join ,join-type ,join-table (on (= ,table-field ,join-field))))
     (format "SELECT ~a FROM ~a ~a JOIN ~a ON ~a.~a = ~a.~a" 
             (string-join (map symbol->string fields) ", ")
             (symbol->string table)
             (symbol->string join-type)
             (symbol->string join-table)
             (symbol->string table)
             (symbol->string table-field)
             (symbol->string join-table)
             (symbol->string join-field))]
    
    ;; Aggregate query
    [`(select (aggregate ,agg-type ,agg-field) (from ,table) (group-by ,group-fields ...))
     (format "SELECT ~a(~a) FROM ~a GROUP BY ~a" 
             (symbol->string agg-type)
             (symbol->string agg-field)
             (symbol->string table)
             (string-join (map symbol->string group-fields) ", "))]
    
    ;; Insert query
    [`(insert (into ,table) (values ([,fields ,values] ...)))
     (format "INSERT INTO ~a (~a) VALUES (~a)" 
             (symbol->string table)
             (string-join (map symbol->string fields) ", ")
             (string-join (map (lambda (v) 
                              (if (string? v) 
                                  (format "~s" v) 
                                  (format "~a" v)))
                            values) 
                         ", "))]
    
    ;; Update query
    [`(update ,table (set ([,fields ,values] ...)) (where (= ,where-field ,where-value)))
     (format "UPDATE ~a SET ~a WHERE ~a = ~s" 
             (symbol->string table)
             (string-join 
              (map (lambda (f v) 
                    (format "~a = ~a" 
                            (symbol->string f) 
                            (if (string? v) (format "~s" v) (format "~a" v))))
                  fields values) 
              ", ")
             (symbol->string where-field)
             where-value)]
    
    ;; Delete query
    [`(delete (from ,table) (where ,condition))
     (format "DELETE FROM ~a WHERE ~a" 
             (symbol->string table)
             (match condition
               [`(= ,field ,value)
                (format "~a = ~s" (symbol->string field) value)]
               [_ "true"]))]
    
    ;; Default - unknown query type
    [_ "INVALID QUERY"]))

;; Test the query processor with various queries
(define queries
  (list
   '(select (fields id name age) (from users))
   
   '(select (fields id name) 
           (from users) 
           (where (= status "active")))
   
   '(select (fields users.id orders.id orders.amount) 
           (from users) 
           (join inner orders (on (= id user_id))))
   
   '(select (aggregate sum amount) (from orders) (group-by user_id status))
   
   '(insert (into users) 
           (values ([name "John Doe"] [age 30] [email "john@example.com"])))
   
   '(update users 
           (set ([name "Jane Doe"] [age 32])) 
           (where (= id 123)))
   
   '(delete (from users) (where (= status "inactive")))
  ))

;; Process each query and display the result
(for ([query queries])
  (printf "Query: ~s\nSQL: ~a\n\n" query (process-query query)))

;; Expected Luau output:
#|
local function process_query(query)
  if query[1] == "select" and #query >= 2 and query[2][1] == "fields" and query[3][1] == "from" then
    -- Simple select
    local fields = {}
    for i=2, #query[2] do
      table.insert(fields, query[2][i])
    end
    local table_name = query[3][2]
    
    if #query == 3 then
      -- SELECT fields FROM table
      return string.format("SELECT %s FROM %s", 
                          table.concat(fields, ", "), 
                          table_name)
    elseif #query == 4 and query[4][1] == "where" and query[4][2][1] == "=" then
      -- SELECT fields FROM table WHERE field = value
      local field = query[4][2][2]
      local value = query[4][2][3]
      return string.format("SELECT %s FROM %s WHERE %s = %s", 
                          table.concat(fields, ", "), 
                          table_name,
                          field,
                          type(value) == "string" and string.format("%q", value) or tostring(value))
    elseif #query == 4 and query[4][1] == "join" then
      -- SELECT fields FROM table JOIN join_table ON table.field = join_table.field
      local join_type = query[4][2]
      local join_table = query[4][3]
      local table_field = query[4][4][2][2]
      local join_field = query[4][4][3]
      
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
  elseif query[1] == "select" and #query >= 3 and query[2][1] == "aggregate" and query[3][1] == "from" and query[4][1] == "group-by" then
    -- Aggregate query
    local agg_type = query[2][2]
    local agg_field = query[2][3]
    local table_name = query[3][2]
    local group_fields = {}
    for i=2, #query[4] do
      table.insert(group_fields, query[4][i])
    end
    
    return string.format("SELECT %s(%s) FROM %s GROUP BY %s", 
                        agg_type,
                        agg_field,
                        table_name,
                        table.concat(group_fields, ", "))
  elseif query[1] == "insert" and #query == 3 and query[2][1] == "into" and query[3][1] == "values" then
    -- INSERT INTO table (fields) VALUES (values)
    local table_name = query[2][2]
    local fields = {}
    local values = {}
    
    for _, pair in ipairs(query[3][2]) do
      table.insert(fields, pair[1])
      table.insert(values, type(pair[2]) == "string" and 
                        string.format("%q", pair[2]) or 
                        tostring(pair[2]))
    end
    
    return string.format("INSERT INTO %s (%s) VALUES (%s)", 
                        table_name,
                        table.concat(fields, ", "),
                        table.concat(values, ", "))
  elseif query[1] == "update" and #query == 4 and query[3][1] == "where" then
    -- UPDATE table SET field = value WHERE field = value
    local table_name = query[2]
    local set_clause = {}
    local fields = {}
    local values = {}
    
    for _, pair in ipairs(query[2][2]) do
      table.insert(fields, pair[1])
      table.insert(values, pair[2])
    end
    
    for i=1, #fields do
      table.insert(set_clause, 
                  string.format("%s = %s", 
                               fields[i], 
                               type(values[i]) == "string" and 
                                  string.format("%q", values[i]) or 
                                  tostring(values[i])))
    end
    
    local where_field = query[3][2][2]
    local where_value = query[3][2][3]
    
    return string.format("UPDATE %s SET %s WHERE %s = %s", 
                        table_name,
                        table.concat(set_clause, ", "),
                        where_field,
                        type(where_value) == "string" and 
                          string.format("%q", where_value) or 
                          tostring(where_value))
  elseif query[1] == "delete" and #query == 3 and query[2][1] == "from" then
    -- DELETE FROM table WHERE condition
    local table_name = query[2][2]
    local condition = query[3][2]
    
    local where_clause = "true"
    if condition[1] == "=" then
      where_clause = string.format("%s = %s", 
                                 condition[2], 
                                 type(condition[3]) == "string" and 
                                    string.format("%q", condition[3]) or 
                                    tostring(condition[3]))
    end
    
    return string.format("DELETE FROM %s WHERE %s", 
                        table_name,
                        where_clause)
  else
    return "INVALID QUERY"
  end
end

local queries = {
  {"select", {"fields", "id", "name", "age"}, {"from", "users"}},
  
  {"select", {"fields", "id", "name"}, 
            {"from", "users"}, 
            {"where", {"=", "status", "active"}}},
  
  {"select", {"fields", "users.id", "orders.id", "orders.amount"}, 
            {"from", "users"}, 
            {"join", "inner", "orders", {"on", {"=", "id", "user_id"}}}},
  
  {"select", {"aggregate", "sum", "amount"}, {"from", "orders"}, {"group-by", "user_id", "status"}},
  
  {"insert", {"into", "users"}, 
            {"values", {{"name", "John Doe"}, {"age", 30}, {"email", "john@example.com"}}}},
  
  {"update", "users", 
            {"set", {{"name", "Jane Doe"}, {"age", 32}}}, 
            {"where", {"=", "id", 123}}},
  
  {"delete", {"from", "users"}, {"where", {"=", "status", "inactive"}}}
}

for _, query in ipairs(queries) do
  print(string.format("Query: %s\nSQL: %s\n", tostring(query), process_query(query)))
end
|# 