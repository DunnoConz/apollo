#lang typed/racket

;; GetService module for Roblox
;; Provides type-safe access to Roblox services
;; 
;; This module implements a type-safe interface to Roblox services, providing:
;; - Type definitions for all Roblox services
;; - Type-safe service access through get-service
;; - Helper functions for common service access
;; - Proper type casting and error handling
;;
;; Example usage:
;; (get-service 'Workspace)  ; Returns a Workspace service instance
;; (get-workspace)          ; Helper function for Workspace service

(require typed/racket/class)

(provide
 (all-defined-out))

;; Define base types
;; Instance is the base type for all Roblox objects
;; @field name: Returns the name of the instance
;; @field parent: Returns the parent instance
;; @field findFirstChild: Finds a child instance by name
;; @field getChildren: Returns all child instances
;; @field getDescendants: Returns all descendant instances
;; @field destroy: Removes the instance from the game
(define-type Instance
  (Class
   [name (-> String)]
   [parent (-> Instance)]
   [findFirstChild (-> String (Option Instance))]
   [getChildren (-> (Vectorof Instance))]
   [getDescendants (-> (Vectorof Instance))]
   [destroy (-> Void)]))

;; Service is a specialized Instance that represents a Roblox service
;; @field name: Returns the name of the service
;; @field parent: Returns the parent instance (usually game)
(define-type Service
  (Class #:implements Instance
   [name (-> String)]
   [parent (-> Instance)]))

;; Core Roblox services
;; Each service type extends the base Service type
;; and provides service-specific functionality
(define-type DataStoreService Service)  ;; Manages data persistence
(define-type Players Service)           ;; Manages player instances
(define-type ReplicatedStorage Service) ;; Stores data replicated to clients
(define-type ServerStorage Service)     ;; Stores data on server only
(define-type ServerScriptService Service) ;; Runs server-side scripts
(define-type StarterPlayer Service)     ;; Manages player settings
(define-type StarterPlayerScripts Service) ;; Scripts that run in new players
(define-type StarterGui Service)        ;; Manages GUI elements for new players
(define-type Workspace Service)         ;; Contains the 3D world
(define-type Lighting Service)          ;; Controls lighting and environment
(define-type RunService Service)        ;; Controls game loop and timing
(define-type UserInputService Service)  ;; Handles user input
(define-type HttpService Service)       ;; Makes HTTP requests
(define-type TweenService Service)      ;; Creates animations
(define-type SoundService Service)      ;; Manages audio
(define-type TeleportService Service)   ;; Handles player teleportation
(define-type MarketplaceService Service) ;; Manages game passes and items
(define-type PathfindingService Service) ;; Handles pathfinding
(define-type PhysicsService Service)    ;; Controls physics behavior
(define-type CollectionService Service) ;; Manages collections of instances
(define-type Chat Service)              ;; Manages chat system
(define-type Teams Service)             ;; Manages teams
(define-type GroupService Service)      ;; Manages Roblox groups
(define-type SocialService Service)     ;; Handles social features
(define-type AnalyticsService Service)  ;; Provides analytics data
(define-type GamePassService Service)   ;; Manages game passes
(define-type BadgeService Service)      ;; Manages badges
(define-type DataStore2 Service)        ;; Alternative data store service

;; Base service implementation
;; Provides default implementations for all Instance methods
;; @field name: Returns empty string by default
;; @field parent: Returns a new instance as parent
;; @field findFirstChild: Returns #f by default
;; @field getChildren: Returns empty vector by default
;; @field getDescendants: Returns empty vector by default
;; @field destroy: Does nothing by default
(define service%
  (class object%
    (super-new)
    (: name (-> String))
    (define/public (name) "")
    (: parent (-> Instance))
    (define/public (parent) (cast (new instance%) Instance))
    (: findFirstChild (-> String (Option Instance)))
    (define/public (findFirstChild name) #f)
    (: getChildren (-> (Vectorof Instance)))
    (define/public (getChildren) (vector))
    (: getDescendants (-> (Vectorof Instance)))
    (define/public (getDescendants) (vector))
    (: destroy (-> Void))
    (define/public (destroy) (void))))

;; Base instance implementation
;; Similar to service% but implements Instance type
;; @see service% for method documentation
(: instance% (Class #:implements Instance
                   [name (-> String)]
                   [parent (-> Instance)]
                   [findFirstChild (-> String (Option Instance))]
                   [getChildren (-> (Vectorof Instance))]
                   [getDescendants (-> (Vectorof Instance))]
                   [destroy (-> Void)]))
(define instance%
  (class object%
    (super-new)
    (: name (-> String))
    (define/public (name) "")
    (: parent (-> Instance))
    (define/public (parent) (cast (new instance%) Instance))
    (: findFirstChild (-> String (Option Instance)))
    (define/public (findFirstChild name) #f)
    (: getChildren (-> (Vectorof Instance)))
    (define/public (getChildren) (vector))
    (: getDescendants (-> (Vectorof Instance)))
    (define/public (getDescendants) (vector))
    (: destroy (-> Void))
    (define/public (destroy) (void))))

;; Service implementations
;; Each service class extends service% and can override methods
;; as needed for service-specific functionality
(define workspace%
  (class service%
    (super-new)))

(define players%
  (class service%
    (super-new)))

(define replicated-storage%
  (class service%
    (super-new)))

(define server-storage%
  (class service%
    (super-new)))

(define server-script-service%
  (class service%
    (super-new)))

(define starter-player%
  (class service%
    (super-new)))

(define starter-player-scripts%
  (class service%
    (super-new)))

(define starter-gui%
  (class service%
    (super-new)))

(define lighting%
  (class service%
    (super-new)))

(define run-service%
  (class service%
    (super-new)))

(define user-input-service%
  (class service%
    (super-new)))

(define http-service%
  (class service%
    (super-new)))

(define tween-service%
  (class service%
    (super-new)))

(define sound-service%
  (class service%
    (super-new)))

(define teleport-service%
  (class service%
    (super-new)))

(define marketplace-service%
  (class service%
    (super-new)))

(define pathfinding-service%
  (class service%
    (super-new)))

(define physics-service%
  (class service%
    (super-new)))

(define collection-service%
  (class service%
    (super-new)))

(define chat%
  (class service%
    (super-new)))

(define teams%
  (class service%
    (super-new)))

(define group-service%
  (class service%
    (super-new)))

(define social-service%
  (class service%
    (super-new)))

(define analytics-service%
  (class service%
    (super-new)))

(define game-pass-service%
  (class service%
    (super-new)))

(define badge-service%
  (class service%
    (super-new)))

(define data-store2%
  (class service%
    (super-new)))

;; GetService function with type safety
;; @param service-name: Symbol representing the service to get
;; @return: Instance of the requested service
;; @throws: Error if service-name is unknown
(: get-service (-> Symbol Service))
(define (get-service service-name)
  (case service-name
    [(DataStoreService) (cast (new data-store2%) DataStoreService)]
    [(Players) (cast (new players%) Players)]
    [(ReplicatedStorage) (cast (new replicated-storage%) ReplicatedStorage)]
    [(ServerStorage) (cast (new server-storage%) ServerStorage)]
    [(ServerScriptService) (cast (new server-script-service%) ServerScriptService)]
    [(StarterPlayer) (cast (new starter-player%) StarterPlayer)]
    [(StarterPlayerScripts) (cast (new starter-player-scripts%) StarterPlayerScripts)]
    [(StarterGui) (cast (new starter-gui%) StarterGui)]
    [(Workspace) (cast (new workspace%) Workspace)]
    [(Lighting) (cast (new lighting%) Lighting)]
    [(RunService) (cast (new run-service%) RunService)]
    [(UserInputService) (cast (new user-input-service%) UserInputService)]
    [(HttpService) (cast (new http-service%) HttpService)]
    [(TweenService) (cast (new tween-service%) TweenService)]
    [(SoundService) (cast (new sound-service%) SoundService)]
    [(TeleportService) (cast (new teleport-service%) TeleportService)]
    [(MarketplaceService) (cast (new marketplace-service%) MarketplaceService)]
    [(PathfindingService) (cast (new pathfinding-service%) PathfindingService)]
    [(PhysicsService) (cast (new physics-service%) PhysicsService)]
    [(CollectionService) (cast (new collection-service%) CollectionService)]
    [(Chat) (cast (new chat%) Chat)]
    [(Teams) (cast (new teams%) Teams)]
    [(GroupService) (cast (new group-service%) GroupService)]
    [(SocialService) (cast (new social-service%) SocialService)]
    [(AnalyticsService) (cast (new analytics-service%) AnalyticsService)]
    [(GamePassService) (cast (new game-pass-service%) GamePassService)]
    [(BadgeService) (cast (new badge-service%) BadgeService)]
    [(DataStore2) (cast (new data-store2%) DataStore2)]
    [else (error (format "Unknown service: ~a" service-name))]))

;; Helper functions for common services
;; Each function returns a properly typed service instance
;; @return: Instance of the requested service type

(: get-data-store-service (-> DataStoreService))
(define (get-data-store-service)
  (cast (get-service 'DataStoreService) DataStoreService))

(: get-players (-> Players))
(define (get-players)
  (cast (get-service 'Players) Players))

(: get-replicated-storage (-> ReplicatedStorage))
(define (get-replicated-storage)
  (cast (get-service 'ReplicatedStorage) ReplicatedStorage))

(: get-server-storage (-> ServerStorage))
(define (get-server-storage)
  (cast (get-service 'ServerStorage) ServerStorage))

(: get-server-script-service (-> ServerScriptService))
(define (get-server-script-service)
  (cast (get-service 'ServerScriptService) ServerScriptService))

(: get-starter-player (-> StarterPlayer))
(define (get-starter-player)
  (cast (get-service 'StarterPlayer) StarterPlayer))

(: get-starter-player-scripts (-> StarterPlayerScripts))
(define (get-starter-player-scripts)
  (cast (get-service 'StarterPlayerScripts) StarterPlayerScripts))

(: get-starter-gui (-> StarterGui))
(define (get-starter-gui)
  (cast (get-service 'StarterGui) StarterGui))

(: get-workspace (-> Workspace))
(define (get-workspace)
  (cast (get-service 'Workspace) Workspace))

(: get-lighting (-> Lighting))
(define (get-lighting)
  (cast (get-service 'Lighting) Lighting))

(: get-run-service (-> RunService))
(define (get-run-service)
  (cast (get-service 'RunService) RunService))

(: get-user-input-service (-> UserInputService))
(define (get-user-input-service)
  (cast (get-service 'UserInputService) UserInputService))

(: get-http-service (-> HttpService))
(define (get-http-service)
  (cast (get-service 'HttpService) HttpService))

(: get-tween-service (-> TweenService))
(define (get-tween-service)
  (cast (get-service 'TweenService) TweenService))

(: get-sound-service (-> SoundService))
(define (get-sound-service)
  (cast (get-service 'SoundService) SoundService))

(: get-teleport-service (-> TeleportService))
(define (get-teleport-service)
  (cast (get-service 'TeleportService) TeleportService))

(: get-marketplace-service (-> MarketplaceService))
(define (get-marketplace-service)
  (cast (get-service 'MarketplaceService) MarketplaceService))

(: get-pathfinding-service (-> PathfindingService))
(define (get-pathfinding-service)
  (cast (get-service 'PathfindingService) PathfindingService))

(: get-physics-service (-> PhysicsService))
(define (get-physics-service)
  (cast (get-service 'PhysicsService) PhysicsService))

(: get-collection-service (-> CollectionService))
(define (get-collection-service)
  (cast (get-service 'CollectionService) CollectionService))

(: get-chat (-> Chat))
(define (get-chat)
  (cast (get-service 'Chat) Chat))

(: get-teams (-> Teams))
(define (get-teams)
  (cast (get-service 'Teams) Teams))

(: get-group-service (-> GroupService))
(define (get-group-service)
  (cast (get-service 'GroupService) GroupService))

(: get-social-service (-> SocialService))
(define (get-social-service)
  (cast (get-service 'SocialService) SocialService))

(: get-analytics-service (-> AnalyticsService))
(define (get-analytics-service)
  (cast (get-service 'AnalyticsService) AnalyticsService))

(: get-game-pass-service (-> GamePassService))
(define (get-game-pass-service)
  (cast (get-service 'GamePassService) GamePassService))

(: get-badge-service (-> BadgeService))
(define (get-badge-service)
  (cast (get-service 'BadgeService) BadgeService))

(: get-data-store2 (-> DataStore2))
(define (get-data-store2)
  (cast (get-service 'DataStore2) DataStore2)) 