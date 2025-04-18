#lang racket

;; Advanced Pattern Matching Example
;; Demonstrates the power of pattern matching with quasiquote patterns

;; Define some data structures
(struct message (type payload timestamp) #:transparent)
(struct user (id name status friends) #:transparent)
(struct game-state (players score time status) #:transparent)

;; Sample data for testing
(define test-users
  (list
   (user 1 "Alice" 'online '(2 3))
   (user 2 "Bob" 'away '(1 3 4))
   (user 3 "Charlie" 'offline '(1 2))
   (user 4 "Diana" 'online '(2))))

(define test-messages
  (list
   (message 'chat "Hello everyone!" 1623451234)
   (message 'system "Server maintenance in 10 minutes" 1623451300)
   (message 'notification "Alice is now online" 1623452000)
   (message 'chat "How is everyone doing?" 1623452100)
   (message 'game-event "Player 2 scored a point" 1623453000)))

(define test-game-states
  (list
   (game-state '(1 2) '(0 0) 0 'waiting)
   (game-state '(1 2) '(1 0) 30 'playing)
   (game-state '(1 2) '(1 1) 60 'playing)
   (game-state '(1 2) '(3 2) 120 'finished)))

;; Basic pattern matching functions

;; Find users with a specific status
(define (find-users-by-status users status)
  (filter (lambda (u) (eq? (user-status u) status)) users))

;; Find messages of a specific type
(define (find-messages-by-type messages type)
  (filter (lambda (m) (eq? (message-type m) type)) messages))

;; Advanced pattern matching with quasiquote

;; Process a message based on its content and structure
(define (process-message msg)
  (match msg
    ;; Match chat messages and extract the payload
    [(message 'chat payload _)
     (string-append "CHAT: " payload)]
    
    ;; Match system messages with specific content patterns using quasiquote
    [`(message 'system ,(? (lambda (p) (regexp-match? #rx"maintenance" p)) payload) ,_)
     (string-append "SYSTEM ALERT: " payload)]
    
    ;; Match notification messages about users coming online
    [`(message 'notification ,(? (lambda (p) (regexp-match? #rx"online" p)) payload) ,_)
     (string-append "USER STATUS: " payload)]
    
    ;; Match game events with specific patterns
    [`(message 'game-event ,(? (lambda (p) (regexp-match? #rx"scored" p)) payload) ,_)
     (string-append "GAME UPDATE: " payload)]
    
    ;; Default case for unmatched messages
    [_ "UNKNOWN MESSAGE"]))

;; Process a game state update using quasiquote patterns
(define (process-game-state state)
  (match state
    ;; Waiting for players
    [`(game-state ,_ ,_ ,_ 'waiting)
     "Game is waiting for players to start"]
    
    ;; Game in progress with tied score
    [`(game-state ,players (,s ,s) ,time 'playing)
     (format "Game tied at ~a with ~a seconds left" s time)]
    
    ;; Game in progress with player 1 winning
    [`(game-state ,players (,s1 ,s2) ,time 'playing) #:when (> s1 s2)
     (format "Player 1 leading ~a-~a with ~a seconds left" s1 s2 time)]
    
    ;; Game in progress with player 2 winning
    [`(game-state ,players (,s1 ,s2) ,time 'playing) #:when (< s1 s2)
     (format "Player 2 leading ~a-~a with ~a seconds left" s1 s2 time)]
    
    ;; Game finished
    [`(game-state ,players (,s1 ,s2) ,_ 'finished) #:when (> s1 s2)
     (format "Game over, Player 1 wins ~a-~a" s1 s2)]
    
    [`(game-state ,players (,s1 ,s2) ,_ 'finished) #:when (< s1 s2)
     (format "Game over, Player 2 wins ~a-~a" s1 s2)]
    
    [`(game-state ,players (,s ,s) ,_ 'finished)
     (format "Game over, tied at ~a-~a" s s)]
    
    ;; Default case
    [_ "Unknown game state"]))

;; Process user data with complex pattern matching
(define (find-mutual-friends users)
  (for*/list ([u1 (in-list users)]
              [u2 (in-list users)]
              #:when (< (user-id u1) (user-id u2))
              #:when (and (member (user-id u2) (user-friends u1))
                          (member (user-id u1) (user-friends u2))))
    (list (user-name u1) (user-name u2))))

;; Complex pattern matching with nested quasiquote patterns
(define (analyze-game-events game-states messages)
  (define events '())
  
  ;; Use quasiquote patterns to match different combinations of states and messages
  (for* ([state game-states]
         [msg messages])
    (match (list state msg)
      ;; Match a game starting
      [`((game-state ,players ,_ ,_ 'waiting)
         (message 'system ,(? (lambda (p) (regexp-match? #rx"start" p)) payload) ,_))
       (set! events (cons `(game-start ,players ,payload) events))]
      
      ;; Match a player scoring
      [`((game-state ,players (,s1 ,s2) ,_ 'playing)
         (message 'game-event ,(? (lambda (p) (regexp-match? #rx"scored" p)) payload) ,_))
       (set! events (cons `(score-update ,players (,s1 ,s2) ,payload) events))]
      
      ;; Match a game ending
      [`((game-state ,players ,score ,_ 'finished)
         (message 'system ,(? (lambda (p) (regexp-match? #rx"finished" p)) payload) ,_))
       (set! events (cons `(game-end ,players ,score ,payload) events))]
      
      ;; No matching event
      [_ (void)]))
  
  events)

;; Complex data transformation using quasiquote patterns
(define (transform-user-data users)
  (map (lambda (user)
         (match user
           ;; Transform online users
           [`(user ,id ,name 'online ,friends)
            `(active-user ,id ,name ,(length friends))]
           
           ;; Transform offline users
           [`(user ,id ,name 'offline ,friends)
            `(inactive-user ,id ,name)]
           
           ;; Transform away users
           [`(user ,id ,name 'away ,friends)
            `(idle-user ,id ,name ,(car friends))]
           
           ;; Default case
           [_ user]))
       users))

;; Generate a report on user activity
(define (generate-user-report users messages)
  (define report '())
  
  ;; Find all chat messages
  (define chat-messages
    (filter (lambda (msg) (eq? (message-type msg) 'chat)) messages))
  
  ;; Count messages per user
  (define user-message-count
    (make-hash))
  
  ;; Use quasiquote patterns to match and process data
  (for* ([user users]
         [msg chat-messages])
    (match msg
      [`(message 'chat ,(? (lambda (p) (regexp-match? 
                                        (regexp (string-append ".*" (user-name user) ".*")) 
                                        p))
                 payload) ,_)
       (hash-update! user-message-count (user-id user) add1 0)]
      [_ (void)]))
  
  ;; Format the report
  (for ([user users])
    (match user
      [`(user ,id ,name ,status ,friends)
       (set! report 
             (cons 
              `(user-activity 
                ,id 
                ,name 
                ,status 
                ,(hash-ref user-message-count id 0)
                ,(if (eq? status 'online) "active" "inactive"))
              report))]))
  
  (reverse report))

;; Test the functions
(define (run-tests)
  (displayln "Testing basic pattern matching functions:")
  (displayln (find-users-by-status test-users 'online))
  (displayln (find-messages-by-type test-messages 'chat))
  
  (displayln "\nTesting advanced pattern matching with quasiquote:")
  (for ([msg test-messages])
    (displayln (process-message msg)))
  
  (displayln "\nTesting game state processing:")
  (for ([state test-game-states])
    (displayln (process-game-state state)))
  
  (displayln "\nFinding mutual friends:")
  (displayln (find-mutual-friends test-users))
  
  (displayln "\nTransforming user data:")
  (displayln (transform-user-data test-users))
  
  (displayln "\nGenerating user report:")
  (displayln (generate-user-report test-users test-messages)))

;; Run the tests
(run-tests)

;; Example of generating Luau code from pattern matching
(define (generate-luau-from-patterns pattern-functions data)
  (string-append
   "-- Generated from Racket pattern matching\n"
   "local function processData(data)\n"
   "  local results = {}\n"
   "  \n"
   "  for i, item in ipairs(data) do\n"
   "    local itemType = item.type\n"
   "    \n"
   "    if itemType == \"user\" then\n"
   "      if item.status == \"online\" then\n"
   "        table.insert(results, {\n"
   "          processedType = \"active_user\",\n"
   "          id = item.id,\n"
   "          name = item.name,\n"
   "          friendCount = #item.friends\n"
   "        })\n"
   "      elseif item.status == \"offline\" then\n"
   "        table.insert(results, {\n"
   "          processedType = \"inactive_user\",\n"
   "          id = item.id,\n"
   "          name = item.name\n"
   "        })\n"
   "      else\n"
   "        table.insert(results, {\n"
   "          processedType = \"other_user\",\n"
   "          id = item.id,\n"
   "          name = item.name,\n"
   "          status = item.status\n"
   "        })\n"
   "      end\n"
   "    elseif itemType == \"message\" then\n"
   "      if item.messageType == \"chat\" then\n"
   "        table.insert(results, {\n"
   "          processedType = \"chat_message\",\n"
   "          content = item.payload,\n"
   "          time = item.timestamp\n"
   "        })\n"
   "      elseif item.messageType == \"system\" and string.find(item.payload, \"maintenance\") then\n"
   "        table.insert(results, {\n"
   "          processedType = \"system_alert\",\n"
   "          content = item.payload,\n"
   "          time = item.timestamp\n"
   "        })\n"
   "      else\n"
   "        table.insert(results, {\n"
   "          processedType = \"other_message\",\n"
   "          content = item.payload,\n"
   "          time = item.timestamp\n"
   "        })\n"
   "      end\n"
   "    elseif itemType == \"game\" then\n"
   "      if item.status == \"playing\" then\n"
   "        local player1Score = item.score[1]\n"
   "        local player2Score = item.score[2]\n"
   "        \n"
   "        if player1Score > player2Score then\n"
   "          table.insert(results, {\n"
   "            processedType = \"game_update\",\n"
   "            status = \"Player 1 leading\",\n"
   "            score = string.format(\"%d-%d\", player1Score, player2Score),\n"
   "            timeLeft = item.time\n"
   "          })\n"
   "        elseif player1Score < player2Score then\n"
   "          table.insert(results, {\n"
   "            processedType = \"game_update\",\n"
   "            status = \"Player 2 leading\",\n"
   "            score = string.format(\"%d-%d\", player1Score, player2Score),\n"
   "            timeLeft = item.time\n"
   "          })\n"
   "        else\n"
   "          table.insert(results, {\n"
   "            processedType = \"game_update\",\n"
   "            status = \"Game tied\",\n"
   "            score = string.format(\"%d-%d\", player1Score, player2Score),\n"
   "            timeLeft = item.time\n"
   "          })\n"
   "        end\n"
   "      elseif item.status == \"finished\" then\n"
   "        table.insert(results, {\n"
   "          processedType = \"game_ended\",\n"
   "          players = item.players,\n"
   "          finalScore = item.score\n"
   "        })\n"
   "      end\n"
   "    end\n"
   "  end\n"
   "  \n"
   "  return results\n"
   "end\n\n"
   
   "-- Example usage\n"
   "local testData = {\n"
   "  {\n"
   "    type = \"user\",\n"
   "    id = 1,\n"
   "    name = \"Alice\",\n"
   "    status = \"online\",\n"
   "    friends = {2, 3}\n"
   "  },\n"
   "  {\n"
   "    type = \"message\",\n"
   "    messageType = \"chat\",\n"
   "    payload = \"Hello everyone!\",\n"
   "    timestamp = 1623451234\n"
   "  },\n"
   "  {\n"
   "    type = \"game\",\n"
   "    players = {1, 2},\n"
   "    score = {3, 2},\n"
   "    time = 120,\n"
   "    status = \"finished\"\n"
   "  }\n"
   "}\n\n"
   
   "local results = processData(testData)\n"
   "for i, result in ipairs(results) do\n"
   "  print(string.format(\"Processed item %d: %s\", i, result.processedType))\n"
   "end\n"))

;; Display the generated Luau code
(displayln "\nGenerated Luau code:")
(displayln (generate-luau-from-patterns '() '())) 