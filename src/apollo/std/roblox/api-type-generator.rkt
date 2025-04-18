#lang racket/base

(require racket/contract
         racket/string
         racket/list
         net/url
         json
         racket/match
         racket/port
         racket/format)

(provide
 (contract-out
  [generate-roblox-types (-> path-string? void?)]
  [fetch-roblox-api (-> jsexpr?)]
  [parse-api-types (-> jsexpr? (listof (list/c symbol? jsexpr?)))]
  [generate-racket-type (-> symbol? jsexpr? string?)]))

;; Custom error types
(struct roblox-api-error exn:fail (type message) #:transparent)
(struct roblox-type-error exn:fail (type message) #:transparent)
(struct roblox-validation-error exn:fail (type message) #:transparent)

;; Error handling for API fetch
(define (handle-api-error e)
  (raise (roblox-api-error "api-fetch" (exn-message e))))

;; Error handling for type generation
(define (handle-type-error e)
  (raise (roblox-type-error "type-generation" (exn-message e))))

;; Error handling for validation
(define (handle-validation-error e)
  (raise (roblox-validation-error "validation" (exn-message e))))

;; Fetch the Roblox API documentation with error handling
(define (fetch-roblox-api)
  (with-handlers ([exn:fail? handle-api-error])
    (define api-url "https://api.roblox.com/docs/v1")
    (define response (get-pure-port (string->url api-url)))
    (define json-data (read-json response))
    (close-input-port response)
    json-data))

;; Parse the API types from the documentation
(define (parse-api-types api-doc)
  (define types (hash-ref api-doc 'types '()))
  (for/list ([type types])
    (list (string->symbol (hash-ref type 'name))
          type)))

;; Enhanced Roblox type to Racket type conversion
(define (roblox-type->racket-type type)
  (match type
    ;; Basic types
    ["string" "String"]
    ["number" "Number"]
    ["boolean" "Boolean"]
    ["table" "HashTable"]
    ["function" "Procedure"]
    ["userdata" "Any"]
    ["nil" "Void"]
    
    ;; Core Roblox types
    ["Instance" "Instance"]
    ["Service" "Service"]
    ["DataModel" "DataModel"]
    ["Workspace" "Workspace"]
    ["ReplicatedStorage" "ReplicatedStorage"]
    ["ServerScriptService" "ServerScriptService"]
    ["StarterGui" "StarterGui"]
    ["StarterPack" "StarterPack"]
    ["StarterPlayer" "StarterPlayer"]
    ["Lighting" "Lighting"]
    ["SoundService" "SoundService"]
    ["Chat" "Chat"]
    ["Players" "Players"]
    ["Teams" "Teams"]
    ["MarketplaceService" "MarketplaceService"]
    ["DataStoreService" "DataStoreService"]
    ["MessagingService" "MessagingService"]
    ["TeleportService" "TeleportService"]
    ["HttpService" "HttpService"]
    ["RunService" "RunService"]
    ["UserInputService" "UserInputService"]
    ["TweenService" "TweenService"]
    ["PathfindingService" "PathfindingService"]
    ["PhysicsService" "PhysicsService"]
    ["CollectionService" "CollectionService"]
    ["ContextActionService" "ContextActionService"]
    ["ReplicatedFirst" "ReplicatedFirst"]
    ["StarterPlayerScripts" "StarterPlayerScripts"]
    ["StarterCharacterScripts" "StarterCharacterScripts"]
    
    ;; Instance types
    ["BasePart" "BasePart"]
    ["Part" "Part"]
    ["WedgePart" "WedgePart"]
    ["CornerWedgePart" "CornerWedgePart"]
    ["TrussPart" "TrussPart"]
    ["VehicleSeat" "VehicleSeat"]
    ["Seat" "Seat"]
    ["SpawnLocation" "SpawnLocation"]
    ["Model" "Model"]
    ["Script" "Script"]
    ["LocalScript" "LocalScript"]
    ["ModuleScript" "ModuleScript"]
    ["StringValue" "StringValue"]
    ["BoolValue" "BoolValue"]
    ["IntValue" "IntValue"]
    ["NumberValue" "NumberValue"]
    ["ObjectValue" "ObjectValue"]
    ["CFrameValue" "CFrameValue"]
    ["Color3Value" "Color3Value"]
    ["Vector3Value" "Vector3Value"]
    ["RayValue" "RayValue"]
    ["BrickColorValue" "BrickColorValue"]
    ["Humanoid" "Humanoid"]
    ["Character" "Character"]
    ["Player" "Player"]
    ["PlayerGui" "PlayerGui"]
    ["Backpack" "Backpack"]
    ["StarterGear" "StarterGear"]
    ["Tool" "Tool"]
    ["HopperBin" "HopperBin"]
    ["Camera" "Camera"]
    ["Terrain" "Terrain"]
    ["Sky" "Sky"]
    ["Light" "Light"]
    ["PointLight" "PointLight"]
    ["SpotLight" "SpotLight"]
    ["SurfaceLight" "SurfaceLight"]
    ["Fire" "Fire"]
    ["Smoke" "Smoke"]
    ["Sparkles" "Sparkles"]
    ["ParticleEmitter" "ParticleEmitter"]
    ["Explosion" "Explosion"]
    ["Sound" "Sound"]
    ["SoundGroup" "SoundGroup"]
    ["RocketPropulsion" "RocketPropulsion"]
    ["BodyMover" "BodyMover"]
    ["BodyPosition" "BodyPosition"]
    ["BodyGyro" "BodyGyro"]
    ["BodyThrust" "BodyThrust"]
    ["BodyForce" "BodyForce"]
    ["BodyAngularVelocity" "BodyAngularVelocity"]
    ["BodyVelocity" "BodyVelocity"]
    ["BodyPosition" "BodyPosition"]
    ["BodyGyro" "BodyGyro"]
    ["BodyMover" "BodyMover"]
    ["BodyThrust" "BodyThrust"]
    ["BodyForce" "BodyForce"]
    ["BodyAngularVelocity" "BodyAngularVelocity"]
    ["BodyVelocity" "BodyVelocity"]
    
    ;; UI types
    ["GuiObject" "GuiObject"]
    ["Frame" "Frame"]
    ["TextLabel" "TextLabel"]
    ["TextButton" "TextButton"]
    ["ImageLabel" "ImageLabel"]
    ["ImageButton" "ImageButton"]
    ["ScrollingFrame" "ScrollingFrame"]
    ["TextBox" "TextBox"]
    ["ViewportFrame" "ViewportFrame"]
    ["ScreenGui" "ScreenGui"]
    ["BillboardGui" "BillboardGui"]
    ["SurfaceGui" "SurfaceGui"]
    ["LayerCollector" "LayerCollector"]
    ["UIGridLayout" "UIGridLayout"]
    ["UIListLayout" "UIListLayout"]
    ["UIPadding" "UIPadding"]
    ["UIScale" "UIScale"]
    ["UICorner" "UICorner"]
    ["UIStroke" "UIStroke"]
    ["UIGradient" "UIGradient"]
    ["UIAspectRatioConstraint" "UIAspectRatioConstraint"]
    ["UISizeConstraint" "UISizeConstraint"]
    ["UITextSizeConstraint" "UITextSizeConstraint"]
    
    ;; Handle array types
    [(regexp #rx"^Array<(.+)>$" (list _ inner-type))
     (format "(Vectorof ~a)" (roblox-type->racket-type inner-type))]
    
    ;; Handle union types
    [(regexp #rx"^(.+)\\|(.+)$" (list _ type1 type2))
     (format "(U ~a ~a)" 
             (roblox-type->racket-type type1)
             (roblox-type->racket-type type2))]
    
    ;; Handle generic types
    [(regexp #rx"^(.+)<(.+)>$" (list _ base-type param-type))
     (format "(~a ~a)" 
             (roblox-type->racket-type base-type)
             (roblox-type->racket-type param-type))]
    
    ;; Handle enum types
    [(regexp #rx"^Enum\\.(.+)$" (list _ enum-name))
     (format "Enum:~a" enum-name)]
    
    ;; Handle event types
    [(regexp #rx"^RBXScriptSignal<(.+)>$" (list _ param-type))
     (format "RBXScriptSignal:~a" (roblox-type->racket-type param-type))]
    
    [_ "Any"]))

;; Type validation
(define (validate-type type-info)
  (with-handlers ([exn:fail? handle-validation-error])
    (define properties (hash-ref type-info 'properties '()))
    (define methods (hash-ref type-info 'methods '()))
    (define events (hash-ref type-info 'events '()))
    (define enums (hash-ref type-info 'enums '()))
    
    ;; Validate properties
    (for ([prop properties])
      (unless (hash-has-key? prop 'name)
        (raise (roblox-validation-error "property" "Property missing name")))
      (unless (hash-has-key? prop 'type)
        (raise (roblox-validation-error "property" "Property missing type")))
      (unless (hash-has-key? prop 'description)
        (hash-set! prop 'description "")))
    
    ;; Validate methods
    (for ([method methods])
      (unless (hash-has-key? method 'name)
        (raise (roblox-validation-error "method" "Method missing name")))
      (unless (hash-has-key? method 'parameters)
        (hash-set! method 'parameters '()))
      (unless (hash-has-key? method 'return-type)
        (hash-set! method 'return-type "Void"))
      (unless (hash-has-key? method 'description)
        (hash-set! method 'description "")))
    
    ;; Validate events
    (for ([event events])
      (unless (hash-has-key? event 'name)
        (raise (roblox-validation-error "event" "Event missing name")))
      (unless (hash-has-key? event 'type)
        (raise (roblox-validation-error "event" "Event missing type")))
      (unless (hash-has-key? event 'description)
        (hash-set! event 'description "")))
    
    ;; Validate enums
    (for ([enum enums])
      (unless (hash-has-key? enum 'name)
        (raise (roblox-validation-error "enum" "Enum missing name")))
      (unless (hash-has-key? enum 'value)
        (raise (roblox-validation-error "enum" "Enum missing value")))
      (unless (hash-has-key? enum 'description)
        (hash-set! enum 'description "")))
    
    #t))

;; Enhanced documentation generation
(define (generate-type-documentation name type-info)
  (validate-type type-info)
  
  (define description (hash-ref type-info 'description ""))
  (define properties (hash-ref type-info 'properties '()))
  (define methods (hash-ref type-info 'methods '()))
  (define events (hash-ref type-info 'events '()))
  (define enums (hash-ref type-info 'enums '()))
  (define deprecated (hash-ref type-info 'deprecated #f))
  (define security (hash-ref type-info 'security #f))
  (define thread-safety (hash-ref type-info 'thread-safety #f))
  (define performance (hash-ref type-info 'performance #f))
  (define examples (hash-ref type-info 'examples '()))
  
  (string-append
   ";; " name "\n"
   ";; " (make-string (string-length name) #\-) "\n"
   (if (non-empty-string? description)
       (format ";; ~a\n" description)
       "")
   "\n"
   (if deprecated
       (format ";; @deprecated ~a\n" deprecated)
       "")
   (if security
       (format ";; @security ~a\n" security)
       "")
   (if thread-safety
       (format ";; @thread-safety ~a\n" thread-safety)
       "")
   (if performance
       (format ";; @performance ~a\n" performance)
       "")
   "\n"
   (if (not (empty? properties))
       (string-append
        ";; Properties:\n"
        (string-join
         (for/list ([prop properties])
           (format ";; - ~a: ~a - ~a~a"
                   (hash-ref prop 'name)
                   (roblox-type->racket-type (hash-ref prop 'type))
                   (hash-ref prop 'description "")
                   (if (hash-ref prop 'deprecated #f)
                       (format " (deprecated: ~a)" (hash-ref prop 'deprecated))
                       "")))
         "\n")
        "\n\n")
       "")
   (if (not (empty? methods))
       (string-append
        ";; Methods:\n"
        (string-join
         (for/list ([method methods])
           (format ";; - ~a: ~a -> ~a - ~a~a"
                   (hash-ref method 'name)
                   (string-join
                    (for/list ([param (hash-ref method 'parameters '())])
                      (format "~a: ~a"
                              (hash-ref param 'name)
                              (roblox-type->racket-type (hash-ref param 'type))))
                    " ")
                   (roblox-type->racket-type (hash-ref method 'return-type "Void"))
                   (hash-ref method 'description "")
                   (if (hash-ref method 'deprecated #f)
                       (format " (deprecated: ~a)" (hash-ref method 'deprecated))
                       "")))
         "\n")
        "\n\n")
       "")
   (if (not (empty? events))
       (string-append
        ";; Events:\n"
        (string-join
         (for/list ([event events])
           (format ";; - ~a: ~a - ~a~a"
                   (hash-ref event 'name)
                   (roblox-type->racket-type (hash-ref event 'type))
                   (hash-ref event 'description "")
                   (if (hash-ref event 'deprecated #f)
                       (format " (deprecated: ~a)" (hash-ref event 'deprecated))
                       "")))
         "\n")
        "\n\n")
       "")
   (if (not (empty? enums))
       (string-append
        ";; Enums:\n"
        (string-join
         (for/list ([enum enums])
           (format ";; - ~a: ~a - ~a~a"
                   (hash-ref enum 'name)
                   (hash-ref enum 'value)
                   (hash-ref enum 'description "")
                   (if (hash-ref enum 'deprecated #f)
                       (format " (deprecated: ~a)" (hash-ref enum 'deprecated))
                       "")))
         "\n")
        "\n\n")
       "")
   (if (not (empty? examples))
       (string-append
        ";; Examples:\n"
        (string-join
         (for/list ([example examples])
           (format ";; ```lua\n;; ~a\n;; ```\n"
                   (hash-ref example 'code)))
         "\n")
        "\n\n")
       "")))

;; Generate method signature
(define (generate-method-signature method)
  (define name (hash-ref method 'name))
  (define params (hash-ref method 'parameters '()))
  (define return-type (hash-ref method 'return-type "Void"))
  (define description (hash-ref method 'description ""))
  
  (string-append
   (format "  [~a (-> ~a ~a)]"
           name
           (string-join 
            (for/list ([param params])
              (format "~a" (roblox-type->racket-type (hash-ref param 'type))))
            " ")
           (roblox-type->racket-type return-type))
   (if (non-empty-string? description)
       (format " ; ~a" description)
       "")))

;; Generate event signature
(define (generate-event-signature event)
  (define name (hash-ref event 'name))
  (define type (hash-ref event 'type))
  (define description (hash-ref event 'description ""))
  
  (string-append
   (format "  [~a RBXScriptSignal:~a]"
           name
           (roblox-type->racket-type type))
   (if (non-empty-string? description)
       (format " ; ~a" description)
       "")))

;; Generate enum definition
(define (generate-enum-definition enum)
  (define name (hash-ref enum 'name))
  (define value (hash-ref enum 'value))
  (define description (hash-ref enum 'description ""))
  
  (string-append
   (format "  [~a ~a]"
           name
           value)
   (if (non-empty-string? description)
       (format " ; ~a" description)
       "")))

;; Generate Racket type definition with inheritance, methods, events, and enums
(define (generate-racket-type name type-info)
  (define properties (hash-ref type-info 'properties '()))
  (define methods (hash-ref type-info 'methods '()))
  (define events (hash-ref type-info 'events '()))
  (define enums (hash-ref type-info 'enums '()))
  (define superclass (hash-ref type-info 'superclass #f))
  
  (string-append
   (generate-type-documentation name type-info)
   (if superclass
       (format "(struct ~a ~a (" (symbol->string name) (symbol->string superclass))
       (format "(struct ~a (" (symbol->string name)))
   (string-join
    (for/list ([prop properties])
      (format "[~a : ~a]"
              (hash-ref prop 'name)
              (roblox-type->racket-type (hash-ref prop 'type))))
    " ")
   ")\n  #:transparent"
   (if (not (empty? methods))
       (string-append "\n  #:methods gen:custom-write\n"
                      "  [(define (write-proc self port mode)\n"
                      "     (fprintf port \"#<~a>\" (object-name self)))]\n"
                      "  #:methods gen:equal+hash\n"
                      "  [(define (equal-proc a b equal?-recur)\n"
                      "     (equal?-recur (object-name a) (object-name b)))\n"
                      "   (define (hash-proc a hash-recur)\n"
                      "     (hash-recur (object-name a)))\n"
                      "   (define (hash2-proc a hash2-recur)\n"
                      "     (hash2-recur (object-name a)))]")
       "")
   "\n  (define/public (get-methods)\n"
   "    (list\n"
   (string-join
    (for/list ([method methods])
      (generate-method-signature method))
    "\n")
   "))\n"
   (if (not (empty? events))
       (string-append
        "  (define/public (get-events)\n"
        "    (list\n"
        (string-join
         (for/list ([event events])
           (generate-event-signature event))
         "\n")
        "))\n")
       "")
   (if (not (empty? enums))
       (string-append
        "  (define/public (get-enums)\n"
        "    (list\n"
        (string-join
         (for/list ([enum enums])
           (generate-enum-definition enum))
         "\n")
        "))\n")
       "")))

;; Type inference
(define (infer-type-from-usage usage)
  (match usage
    [(regexp #rx"^new\\s+(\\w+)" (list _ type)) type]
    [(regexp #rx"^Instance\\.new\\s*\\(\\s*\"(\\w+)\"" (list _ type)) type]
    [(regexp #rx"^game:GetService\\s*\\(\\s*\"(\\w+)\"" (list _ type)) type]
    [_ #f]))

(define (infer-type-from-doc doc)
  (match doc
    [(regexp #rx"@type\\s+(\\w+)" (list _ type)) type]
    [(regexp #rx"@return\\s+(\\w+)" (list _ type)) type]
    [(regexp #rx"@param\\s+\\w+\\s+(\\w+)" (list _ type)) type]
    [_ #f]))

;; Custom type support
(define custom-types (make-hash))

(define (register-custom-type name type-info)
  (hash-set! custom-types name type-info))

(define (get-custom-type name)
  (hash-ref custom-types name #f))

(define (generate-custom-type name type-info)
  (define base-type (hash-ref type-info 'base-type "Any"))
  (define properties (hash-ref type-info 'properties '()))
  (define methods (hash-ref type-info 'methods '()))
  (define events (hash-ref type-info 'events '()))
  (define enums (hash-ref type-info 'enums '()))
  
  (string-append
   (format "(struct ~a ~a (" name base-type)
   (string-join
    (for/list ([prop properties])
      (format "[~a : ~a]"
              (hash-ref prop 'name)
              (roblox-type->racket-type (hash-ref prop 'type))))
    " ")
   ")\n  #:transparent"
   (if (not (empty? methods))
       (string-append "\n  #:methods gen:custom-write\n"
                      "  [(define (write-proc self port mode)\n"
                      "     (fprintf port \"#<~a>\" (object-name self)))]\n"
                      "  #:methods gen:equal+hash\n"
                      "  [(define (equal-proc a b equal?-recur)\n"
                      "     (equal?-recur (object-name a) (object-name b)))\n"
                      "   (define (hash-proc a hash-recur)\n"
                      "     (hash-recur (object-name a)))\n"
                      "   (define (hash2-proc a hash2-recur)\n"
                      "     (hash2-recur (object-name a)))]")
       "")
   "\n  (define/public (get-methods)\n"
   "    (list\n"
   (string-join
    (for/list ([method methods])
      (generate-method-signature method))
    "\n")
   "))\n"
   (if (not (empty? events))
       (string-append
        "  (define/public (get-events)\n"
        "    (list\n"
        (string-join
         (for/list ([event events])
           (generate-event-signature event))
         "\n")
        "))\n")
       "")
   (if (not (empty? enums))
       (string-append
        "  (define/public (get-enums)\n"
        "    (list\n"
        (string-join
         (for/list ([enum enums])
           (generate-enum-definition enum))
         "\n")
        "))\n")
       "")
   ")"))

;; Type alias support
(define type-aliases (make-hash))

(define (register-type-alias name type)
  (hash-set! type-aliases name type))

(define (get-type-alias name)
  (hash-ref type-aliases name #f))

;; Type union support
(define (make-type-union types)
  (format "(U ~a)" (string-join (map roblox-type->racket-type types) " ")))

;; Type intersection support
(define (make-type-intersection types)
  (format "(∩ ~a)" (string-join (map roblox-type->racket-type types) " ")))

;; Type narrowing support
(define (narrow-type type predicate)
  (format "(∩ ~a ~a)" (roblox-type->racket-type type) predicate))

;; Type guard support
(define (make-type-guard type predicate)
  (format "(-> ~a Boolean)" (roblox-type->racket-type type)))

;; Main function to generate types with error handling
(define (generate-roblox-types output-path)
  (with-handlers ([exn:fail? (λ (e) (error 'generate-types "Failed to generate types: ~a" (exn-message e)))])
    (define api-doc (fetch-roblox-api))
    (define types (parse-api-types api-doc))
    
    (with-output-to-file output-path
      (lambda ()
        (displayln "#lang typed/racket")
        (displayln "")
        (displayln ";; Generated Roblox API types")
        (displayln ";; This file is automatically generated - DO NOT EDIT")
        (displayln "")
        (displayln "(require typed/racket/class)")
        (displayln "")
        
        ;; Generate base types first
        (for ([type types]
              #:when (member (first type) '(Instance Service)))
          (displayln (generate-racket-type (first type) (second type)))
          (displayln ""))
        
        ;; Generate all other types
        (for ([type types]
              #:unless (member (first type) '(Instance Service)))
          (displayln (generate-racket-type (first type) (second type)))
          (displayln "")))))) 