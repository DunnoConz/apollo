#lang racket

;; Turn-Based Combat System Example
;; Demonstrates a simple RPG-style turn-based combat system with functional state management

;; Define character and monster structures
(struct character (name hp max-hp attack defense magic items) #:transparent)
(struct monster (name hp max-hp attack defense weakness) #:transparent)

;; Define combat state
(struct combat-state (character monsters turn log) #:transparent)

;; Create a new character
(define (make-character name attack defense magic)
  (character name 100 100 attack defense magic '()))

;; Create a new monster
(define (make-monster name hp attack defense weakness)
  (monster name hp hp attack defense weakness))

;; Initialize a new combat state
(define (start-combat character monsters)
  (combat-state character monsters 0 '()))

;; Add a message to the combat log
(define (add-log-message state message)
  (struct-copy combat-state state
               [log (cons message (combat-state-log state))]))

;; Process a player action using pattern matching
(define (process-action state action)
  (match action
    ;; Basic attack
    [`(attack ,target-index)
     (let* ([char (combat-state-character state)]
            [monsters (combat-state-monsters state)]
            [target (list-ref monsters target-index)]
            
            ;; Calculate damage
            [damage (max 1 (- (character-attack char) (monster-defense target)))]
            
            ;; Apply damage to target
            [new-hp (max 0 (- (monster-hp target) damage))]
            [updated-target (struct-copy monster target [hp new-hp])]
            
            ;; Update monster list
            [updated-monsters (list-set monsters target-index updated-target)]
            
            ;; Update log
            [log-message (format "~a attacks ~a for ~a damage!" 
                               (character-name char) 
                               (monster-name target)
                               damage)]
            [state-with-log (add-log-message state log-message)])
       
       ;; Return updated state with new monster list
       (struct-copy combat-state state-with-log
                   [monsters updated-monsters]
                   [turn (add1 (combat-state-turn state))]))]
    
    ;; Magic attack (uses weakness system)
    [`(cast ,spell-name ,target-index)
     (let* ([char (combat-state-character state)]
            [monsters (combat-state-monsters state)]
            [target (list-ref monsters target-index)]
            
            ;; Base magic damage
            [base-damage (character-magic char)]
            
            ;; Check if spell matches monster weakness for bonus damage
            [weakness-multiplier (if (eq? spell-name (monster-weakness target)) 2.0 1.0)]
            [damage (exact-floor (* base-damage weakness-multiplier))]
            
            ;; Apply damage to target
            [new-hp (max 0 (- (monster-hp target) damage))]
            [updated-target (struct-copy monster target [hp new-hp])]
            
            ;; Update monster list
            [updated-monsters (list-set monsters target-index updated-target)]
            
            ;; Update log with weakness bonus message if applicable
            [weakness-msg (if (> weakness-multiplier 1.0) " It's super effective!" "")]
            [log-message (format "~a casts ~a on ~a for ~a damage!~a" 
                               (character-name char)
                               spell-name
                               (monster-name target)
                               damage
                               weakness-msg)]
            [state-with-log (add-log-message state log-message)])
       
       ;; Return updated state with new monster list
       (struct-copy combat-state state-with-log
                   [monsters updated-monsters]
                   [turn (add1 (combat-state-turn state))]))]
    
    ;; Use an item from inventory
    [`(use-item ,item-name)
     (let* ([char (combat-state-character state)]
            [items (character-items char)]
            
            ;; Check if character has the item
            [has-item? (member item-name items)]
            
            ;; If no item, return state with error message
            [state (if has-item?
                      state
                      (add-log-message state 
                                     (format "~a doesn't have ~a in inventory!" 
                                           (character-name char)
                                           item-name)))])
       
       ;; If we have the item, apply its effect
       (if has-item?
           (match item-name
             ;; Potion heals 30 HP
             ["potion"
              (let* ([heal-amount 30]
                    [new-hp (min (character-max-hp char) 
                                (+ (character-hp char) heal-amount))]
                    [updated-char (struct-copy character char 
                                             [hp new-hp]
                                             [items (remove item-name items)])]
                    [log-message (format "~a uses a potion and recovers ~a HP!" 
                                       (character-name char)
                                       heal-amount)]
                    [state-with-log (add-log-message state log-message)])
                
                ;; Return updated state
                (struct-copy combat-state state-with-log
                           [character updated-char]
                           [turn (add1 (combat-state-turn state))]))]
             
             ;; Bomb damages all enemies
             ["bomb"
              (let* ([damage 15]
                    [monsters (combat-state-monsters state)]
                    
                    ;; Apply damage to all monsters
                    [updated-monsters
                     (map (lambda (m) 
                           (struct-copy monster m
                                      [hp (max 0 (- (monster-hp m) damage))]))
                         monsters)]
                    
                    ;; Update character's inventory
                    [updated-char (struct-copy character char
                                             [items (remove item-name items)])]
                    
                    ;; Update log
                    [log-message (format "~a throws a bomb for ~a damage to all enemies!" 
                                       (character-name char)
                                       damage)]
                    [state-with-log (add-log-message state log-message)])
                
                ;; Return updated state
                (struct-copy combat-state state-with-log
                           [character updated-char]
                           [monsters updated-monsters]
                           [turn (add1 (combat-state-turn state))]))]
             
             ;; Unknown item
             [_ state])
           
           ;; If no item, just return the state with error message
           state))]
    
    ;; Default - return unchanged state with error message
    [_ (add-log-message state "Invalid action!")]))

;; Process monster turn
(define (monster-turn state)
  (let* ([char (combat-state-character state)]
         [monsters (combat-state-monsters state)]
         
         ;; Only alive monsters attack
         [living-monsters (filter (lambda (m) (> (monster-hp m) 0)) monsters)])
    
    ;; If no living monsters, combat is over
    (if (null? living-monsters)
        state
        
        ;; Each monster attacks
        (foldl 
         (lambda (monster state)
           (let* ([damage (max 1 (- (monster-attack monster) (character-defense char)))]
                 [new-hp (max 0 (- (character-hp char) damage))]
                 [updated-char (struct-copy character char [hp new-hp])]
                 
                 ;; Add message to log
                 [log-message (format "~a attacks ~a for ~a damage!" 
                                     (monster-name monster)
                                     (character-name char)
                                     damage)]
                 [state-with-log (add-log-message state log-message)])
             
             ;; Return updated state
             (struct-copy combat-state state-with-log
                        [character updated-char])))
         
         state
         living-monsters))))

;; Check if combat is over
(define (combat-over? state)
  (let* ([char (combat-state-character state)]
         [monsters (combat-state-monsters state)]
         [char-dead? (<= (character-hp char) 0)]
         [monsters-dead? (andmap (lambda (m) (<= (monster-hp m) 0)) monsters)])
    (or char-dead? monsters-dead?)))

;; Get result of combat (who won)
(define (combat-result state)
  (let* ([char (combat-state-character state)]
         [monsters (combat-state-monsters state)]
         [char-dead? (<= (character-hp char) 0)]
         [monsters-dead? (andmap (lambda (m) (<= (monster-hp m) 0)) monsters)])
    (cond
      [char-dead? 'monsters-win]
      [monsters-dead? 'player-wins]
      [else 'combat-ongoing])))

;; Run a sample combat
(define (run-sample-combat)
  (let* ([hero (make-character "Hero" 20 10 25)]
         [hero-with-items (struct-copy character hero [items '("potion" "potion" "bomb")])]
         
         [monsters (list
                    (make-monster "Goblin" 40 15 5 'fire)
                    (make-monster "Skeleton" 60 12 8 'light)
                    (make-monster "Slime" 30 10 3 'ice))]
         
         [initial-state (start-combat hero-with-items monsters)]
         
         ;; List of actions to take
         [actions (list
                   '(attack 0)              ; Attack goblin
                   '(cast fire 0)           ; Cast fire on goblin (weakness)
                   '(use-item "potion")     ; Heal with potion
                   '(attack 1)              ; Attack skeleton
                   '(cast light 1)          ; Cast light on skeleton (weakness)
                   '(use-item "bomb")       ; Use bomb on all enemies
                   '(attack 2)              ; Attack slime
                   '(cast ice 2))])         ; Cast ice on slime (weakness)
    
    ;; Process each action in sequence (alternating with monster turns)
    (let loop ([state initial-state]
               [remaining-actions actions])
      
      ;; Display current state
      (printf "Turn ~a:~n" (combat-state-turn state))
      (display-combat-state state)
      
      (cond
        ;; If combat is over, display result
        [(combat-over? state)
         (printf "~nCombat over! Result: ~a~n" (combat-result state))
         (display-combat-log state)]
        
        ;; If no more actions, end
        [(null? remaining-actions)
         (printf "~nNo more actions. Combat continues...~n")]
        
        ;; Process next action
        [else
         (let* ([action (first remaining-actions)]
                [state-after-action (process-action state action)]
                [state-after-monster-turn (monster-turn state-after-action)])
           (printf "~nAction: ~a~n" action)
           (display-combat-log state-after-monster-turn)
           (loop state-after-monster-turn (rest remaining-actions)))]))))

;; Display current combat state
(define (display-combat-state state)
  (let ([char (combat-state-character state)]
        [monsters (combat-state-monsters state)])
    
    ;; Display character info
    (printf "~a: HP ~a/~a, ATK ~a, DEF ~a, MAG ~a~n" 
           (character-name char)
           (character-hp char)
           (character-max-hp char)
           (character-attack char)
           (character-defense char)
           (character-magic char))
    
    ;; Display inventory
    (printf "Items: ~a~n" (character-items char))
    
    ;; Display monsters
    (printf "Monsters:~n")
    (for ([m monsters]
          [i (in-naturals)])
      (printf "  ~a. ~a: HP ~a/~a, ATK ~a, DEF ~a, Weakness: ~a~n"
             i
             (monster-name m)
             (monster-hp m)
             (monster-max-hp m)
             (monster-attack m)
             (monster-defense m)
             (monster-weakness m)))))

;; Display combat log (most recent messages first)
(define (display-combat-log state)
  (let ([log (reverse (combat-state-log state))])
    (printf "Combat log:~n")
    (for ([msg log])
      (printf "  ~a~n" msg))))

;; Run the sample combat
(run-sample-combat) 