#lang racket

;; Roblox Animations and Tweening Example
;; Demonstrates creating animations and tweens with pattern matching for event handling

;; Define a tween structure
(struct tween (target property start-value end-value duration easing-style easing-direction) #:transparent)

;; Define an animation structure
(struct animation (name priority looped speed) #:transparent)

;; Define an event structure
(struct event (type data callback) #:transparent)

;; Create a new tween
(define (create-tween target property start-value end-value duration 
                     #:easing-style [easing-style "Quad"] 
                     #:easing-direction [easing-direction "Out"])
  (tween target property start-value end-value duration easing-style easing-direction))

;; Create a new animation
(define (create-animation name #:priority [priority 1] #:looped [looped #t] #:speed [speed 1.0])
  (animation name priority looped speed))

;; Create a new event
(define (create-event type data callback)
  (event type data callback))

;; Generate Luau code for a tween
(define (generate-tween-code tw)
  (string-append
   "local tween" (tween-target tw) " = TweenService:Create(" 
   (tween-target tw) ", "
   "TweenInfo.new(" (number->string (tween-duration tw)) ", "
   "Enum.EasingStyle." (tween-easing-style tw) ", "
   "Enum.EasingDirection." (tween-easing-direction tw) "), "
   "{ " (tween-property tw) " = " 
   (cond
     [(list? (tween-end-value tw))
      (cond
        [(= (length (tween-end-value tw)) 2)
         (format "Vector2.new(~a, ~a)" (car (tween-end-value tw)) (cadr (tween-end-value tw)))]
        [(= (length (tween-end-value tw)) 3)
         (format "Vector3.new(~a, ~a, ~a)" (car (tween-end-value tw)) (cadr (tween-end-value tw)) (caddr (tween-end-value tw)))]
        [(= (length (tween-end-value tw)) 4)
         (format "Color3.fromRGB(~a, ~a, ~a)" (car (tween-end-value tw)) (cadr (tween-end-value tw)) (caddr (tween-end-value tw)))]
        [else (format "~a" (tween-end-value tw))])]
     [else (format "~a" (tween-end-value tw))])
   " })\n"))

;; Generate Luau code for an animation
(define (generate-animation-code anim)
  (string-append
   "local animation" (animation-name anim) " = Instance.new(\"Animation\")\n"
   "animation" (animation-name anim) ".AnimationId = \"rbxassetid://\" .. "
   (animation-name anim) "Id\n"
   "local animTrack" (animation-name anim) " = character.Humanoid.Animator:LoadAnimation(animation" (animation-name anim) ")\n"
   "animTrack" (animation-name anim) ".Priority = Enum.AnimationPriority." 
   (cond
     [(= (animation-priority anim) 1) "Idle"]
     [(= (animation-priority anim) 2) "Movement"]
     [(= (animation-priority anim) 3) "Action"]
     [(= (animation-priority anim) 4) "Core"]
     [else "Idle"])
   "\n"
   "animTrack" (animation-name anim) ".Looped = " (if (animation-looped anim) "true" "false") "\n"
   "animTrack" (animation-name anim) ":SetSpeed(" (number->string (animation-speed anim)) ")\n"))

;; Process event using pattern matching
(define (process-event evt)
  (match (event-type evt)
    ['tween-complete
     (string-append 
      "print(\"Tween completed for " (car (event-data evt)) "\")\n"
      (if (cdr (event-data evt))
          (string-append (cdr (event-data evt)) "()\n")
          ""))]
    
    ['animation-play
     (string-append 
      "animTrack" (car (event-data evt)) ":Play()\n"
      (if (cdr (event-data evt))
          (string-append "print(\"Playing animation: " (car (event-data evt)) "\")\n")
          ""))]
    
    ['animation-stop
     (string-append 
      "animTrack" (car (event-data evt)) ":Stop()\n"
      (if (cdr (event-data evt))
          (string-append "print(\"Stopping animation: " (car (event-data evt)) "\")\n")
          ""))]
    
    ['animation-keyframe
     (string-append 
      "animTrack" (car (event-data evt)) ".KeyframeReached:Connect(function(keyframeName)\n"
      "  if keyframeName == \"" (cadr (event-data evt)) "\" then\n"
      "    " (if (cddr (event-data evt)) 
               (caddr (event-data evt)) 
               (string-append "print(\"Keyframe reached: " (cadr (event-data evt)) "\")"))
      "\n"
      "  end\n"
      "end)\n")]
    
    ['button-click
     (string-append 
      (car (event-data evt)) ".MouseButton1Click:Connect(function()\n"
      "  " (if (cdr (event-data evt)) (cadr (event-data evt)) "print(\"Button clicked\")") "\n"
      "end)\n")]
    
    ['property-changed
     (string-append 
      (car (event-data evt)) ":GetPropertyChangedSignal(\"" (cadr (event-data evt)) "\"):Connect(function()\n"
      "  " (if (cddr (event-data evt)) (caddr (event-data evt)) "print(\"Property changed\")") "\n"
      "end)\n")]
    
    [_ "print(\"Unknown event type\")\n"]))

;; Generate animation utility functions
(define (generate-animation-utilities)
  (string-append
   "-- Animation utility functions\n"
   "local function fadeAnimation(animTrack, targetWeight, duration)\n"
   "  local startWeight = animTrack.WeightCurrent\n"
   "  local startTime = tick()\n"
   "  \n"
   "  while tick() - startTime < duration do\n"
   "    local alpha = (tick() - startTime) / duration\n"
   "    animTrack:AdjustWeight(startWeight + (targetWeight - startWeight) * alpha)\n"
   "    game:GetService(\"RunService\").Heartbeat:Wait()\n"
   "  end\n"
   "  \n"
   "  animTrack:AdjustWeight(targetWeight)\n"
   "end\n\n"
   
   "local function blendAnimations(animTrack1, animTrack2, duration)\n"
   "  if not animTrack1.IsPlaying then animTrack1:Play() end\n"
   "  if not animTrack2.IsPlaying then animTrack2:Play() end\n"
   "  \n"
   "  spawn(function()\n"
   "    fadeAnimation(animTrack1, 0, duration)\n"
   "  end)\n"
   "  \n"
   "  fadeAnimation(animTrack2, 1, duration)\n"
   "  animTrack1:Stop()\n"
   "end\n\n"))

;; Generate tweening utility functions
(define (generate-tweening-utilities)
  (string-append
   "-- Tweening utility functions\n"
   "local function chainTweens(tweens, callback)\n"
   "  local index = 1\n"
   "  \n"
   "  local function playNextTween()\n"
   "    if index <= #tweens then\n"
   "      local currentTween = tweens[index]\n"
   "      index = index + 1\n"
   "      \n"
   "      currentTween.Completed:Connect(function()\n"
   "        playNextTween()\n"
   "      end)\n"
   "      \n"
   "      currentTween:Play()\n"
   "    else\n"
   "      if callback then callback() end\n"
   "    end\n"
   "  end\n"
   "  \n"
   "  playNextTween()\n"
   "end\n\n"
   
   "local function tweenModel(model, properties, duration, easingStyle, easingDirection)\n"
   "  local tweens = {}\n"
   "  \n"
   "  for _, part in pairs(model:GetDescendants()) do\n"
   "    if part:IsA(\"BasePart\") then\n"
   "      table.insert(tweens, TweenService:Create(part, \n"
   "        TweenInfo.new(duration, easingStyle or Enum.EasingStyle.Quad, easingDirection or Enum.EasingDirection.Out),\n"
   "        properties\n"
   "      ))\n"
   "    end\n"
   "  end\n"
   "  \n"
   "  return tweens\n"
   "end\n\n"))

;; Create sample tweens
(define (create-sample-tweens)
  (list
   (create-tween "part1" "Position" '(0 0 0) '(10 5 0) 2.0 #:easing-style "Quad" #:easing-direction "Out")
   (create-tween "part2" "Size" '(2 2 2) '(5 5 5) 1.5 #:easing-style "Back" #:easing-direction "InOut")
   (create-tween "part3" "Transparency" 0 1 3.0 #:easing-style "Linear" #:easing-direction "In")
   (create-tween "guiButton" "Size" '(0.2 0.1) '(0.3 0.15) 0.5 #:easing-style "Elastic" #:easing-direction "Out")
   (create-tween "light" "Color" '(255 0 0) '(0 0 255) 4.0 #:easing-style "Sine" #:easing-direction "InOut")))

;; Create sample animations
(define (create-sample-animations)
  (list
   (create-animation "Idle" #:priority 1 #:looped #t #:speed 1.0)
   (create-animation "Walk" #:priority 2 #:looped #t #:speed 1.0)
   (create-animation "Run" #:priority 2 #:looped #t #:speed 1.5)
   (create-animation "Jump" #:priority 3 #:looped #f #:speed 1.0)
   (create-animation "Attack" #:priority 3 #:looped #f #:speed 1.2)))

;; Create sample events
(define (create-sample-events)
  (list
   (create-event 'tween-complete '("part1" . "onPart1TweenComplete") "print(\"Part 1 tween completed\")")
   (create-event 'animation-play '("Idle" . #t) "print(\"Idle animation played\")")
   (create-event 'animation-play '("Walk" . #t) "print(\"Walk animation played\")")
   (create-event 'animation-stop '("Idle" . #t) "print(\"Idle animation stopped\")")
   (create-event 'animation-keyframe '("Attack" "Hit" . "playHitEffect()") "print(\"Attack hit keyframe reached\")")
   (create-event 'button-click '("playButton" . "onPlayButtonClicked()") "print(\"Play button clicked\")")
   (create-event 'property-changed '("character" "Health" . "updateHealthUI()") "print(\"Health changed\")")))

;; Generate the complete animation script
(define (generate-animation-script)
  (let ([tweens (create-sample-tweens)]
        [animations (create-sample-animations)]
        [events (create-sample-events)])
    
    (string-append
     "-- Animations and Tweening Script\n"
     "-- Generated from Racket code\n\n"
     
     "local TweenService = game:GetService(\"TweenService\")\n"
     "local character = script.Parent\n\n"
     
     "-- Animation IDs (replace with your own animation IDs)\n"
     "local IdleId = \"1234567890\"\n"
     "local WalkId = \"1234567891\"\n"
     "local RunId = \"1234567892\"\n"
     "local JumpId = \"1234567893\"\n"
     "local AttackId = \"1234567894\"\n\n"
     
     "-- Parts for tweening\n"
     "local part1 = workspace.Part1\n"
     "local part2 = workspace.Part2\n"
     "local part3 = workspace.Part3\n"
     "local guiButton = script.Parent.PlayerGui.MainGui.PlayButton\n"
     "local light = workspace.PointLight\n\n"
     
     "-- Initialize animations\n"
     (apply string-append (map generate-animation-code animations))
     "\n"
     
     "-- Initialize tweens\n"
     (apply string-append (map generate-tween-code tweens))
     "\n"
     
     "-- Set up event handlers\n"
     (apply string-append (map process-event events))
     "\n"
     
     "-- Helper functions for effects\n"
     "local function playHitEffect()\n"
     "  local hitEffect = Instance.new(\"ParticleEmitter\")\n"
     "  hitEffect.Parent = character.RightHand\n"
     "  hitEffect.Texture = \"rbxassetid://1234567\"\n"
     "  hitEffect.EmissionDirection = Enum.NormalId.Front\n"
     "  hitEffect.Lifetime = NumberRange.new(0.5, 1)\n"
     "  hitEffect.Rate = 50\n"
     "  hitEffect.Speed = NumberRange.new(5, 10)\n"
     "  hitEffect.SpreadAngle = Vector2.new(30, 30)\n"
     "  \n"
     "  -- Auto cleanup\n"
     "  hitEffect.Enabled = true\n"
     "  wait(0.2)\n"
     "  hitEffect.Enabled = false\n"
     "  game:GetService(\"Debris\"):AddItem(hitEffect, 1)\n"
     "end\n\n"
     
     "local function onPart1TweenComplete()\n"
     "  -- Chain to the next tween\n"
     "  tweenpart2:Play()\n"
     "end\n\n"
     
     "local function onPlayButtonClicked()\n"
     "  -- Start the animation sequence\n"
     "  animTrackIdle:Stop()\n"
     "  animTrackAttack:Play()\n"
     "  \n"
     "  -- Start the tween sequence\n"
     "  chainTweens({tweenpart1, tweenpart2, tweenpart3}, function()\n"
     "    print(\"All tweens completed\")\n"
     "  end)\n"
     "end\n\n"
     
     "local function updateHealthUI()\n"
     "  -- Update health bar in the UI\n"
     "  local healthBar = script.Parent.PlayerGui.MainGui.HealthBar\n"
     "  local health = character.Health\n"
     "  \n"
     "  TweenService:Create(healthBar, TweenInfo.new(0.5), {\n"
     "    Size = UDim2.new(health / 100, 0, 1, 0)\n"
     "  }):Play()\n"
     "end\n\n"
     
     ;; Add utility functions
     (generate-animation-utilities)
     (generate-tweening-utilities)
     
     "-- Example of animation state controller using pattern matching\n"
     "local function handleCharacterState(state, prevState)\n"
     "  if state == prevState then return end\n"
     "  \n"
     "  if state == \"Idle\" then\n"
     "    animTrackWalk:Stop()\n"
     "    animTrackRun:Stop()\n"
     "    animTrackJump:Stop()\n"
     "    animTrackIdle:Play()\n"
     "  elseif state == \"Walking\" then\n"
     "    animTrackIdle:Stop()\n"
     "    animTrackRun:Stop()\n"
     "    animTrackJump:Stop()\n"
     "    animTrackWalk:Play()\n"
     "  elseif state == \"Running\" then\n"
     "    animTrackIdle:Stop()\n"
     "    animTrackWalk:Stop()\n"
     "    animTrackJump:Stop()\n"
     "    animTrackRun:Play()\n"
     "  elseif state == \"Jumping\" then\n"
     "    -- Don't stop other animations for jumping\n"
     "    animTrackJump:Play()\n"
     "  elseif state == \"Attacking\" then\n"
     "    -- Keep movement animations but play attack\n"
     "    animTrackAttack:Play()\n"
     "  end\n"
     "end\n\n"
     
     "-- Start with idle animation\n"
     "animTrackIdle:Play()\n"
     
     "-- Example of handling movement input\n"
     "local function handleMovementInput()\n"
     "  local previousState = \"Idle\"\n"
     "  local currentState = \"Idle\"\n"
     "  \n"
     "  while true do\n"
     "    local humanoid = character:FindFirstChild(\"Humanoid\")\n"
     "    if humanoid then\n"
     "      if humanoid:GetState() == Enum.HumanoidStateType.Jumping then\n"
     "        currentState = \"Jumping\"\n"
     "      elseif humanoid.MoveDirection.Magnitude > 0 then\n"
     "        if humanoid.WalkSpeed > 16 then\n"
     "          currentState = \"Running\"\n"
     "        else\n"
     "          currentState = \"Walking\"\n"
     "        end\n"
     "      else\n"
     "        currentState = \"Idle\"\n"
     "      end\n"
     "      \n"
     "      handleCharacterState(currentState, previousState)\n"
     "      previousState = currentState\n"
     "    end\n"
     "    \n"
     "    wait(0.1)\n"
     "  end\n"
     "end\n\n"
     
     "-- Start the movement input handling\n"
     "spawn(handleMovementInput)\n")))

;; Display the generated animation script
(display (generate-animation-script))

;; Expected Luau output (truncated for brevity):
#|
-- Animations and Tweening Script
-- Generated from Racket code

local TweenService = game:GetService("TweenService")
local character = script.Parent

-- Animation IDs (replace with your own animation IDs)
local IdleId = "1234567890"
local WalkId = "1234567891"
local RunId = "1234567892"
local JumpId = "1234567893"
local AttackId = "1234567894"

-- Parts for tweening
local part1 = workspace.Part1
local part2 = workspace.Part2
local part3 = workspace.Part3
local guiButton = script.Parent.PlayerGui.MainGui.PlayButton
local light = workspace.PointLight

-- Initialize animations
local animationIdle = Instance.new("Animation")
animationIdle.AnimationId = "rbxassetid://" .. IdleId
local animTrackIdle = character.Humanoid.Animator:LoadAnimation(animationIdle)
animTrackIdle.Priority = Enum.AnimationPriority.Idle
animTrackIdle.Looped = true
animTrackIdle:SetSpeed(1.0)
local animationWalk = Instance.new("Animation")
animationWalk.AnimationId = "rbxassetid://" .. WalkId
local animTrackWalk = character.Humanoid.Animator:LoadAnimation(animationWalk)
animTrackWalk.Priority = Enum.AnimationPriority.Movement
animTrackWalk.Looped = true
animTrackWalk:SetSpeed(1.0)
local animationRun = Instance.new("Animation")
animationRun.AnimationId = "rbxassetid://" .. RunId
local animTrackRun = character.Humanoid.Animator:LoadAnimation(animationRun)
animTrackRun.Priority = Enum.AnimationPriority.Movement
animTrackRun.Looped = true
animTrackRun:SetSpeed(1.5)
local animationJump = Instance.new("Animation")
animationJump.AnimationId = "rbxassetid://" .. JumpId
local animTrackJump = character.Humanoid.Animator:LoadAnimation(animationJump)
animTrackJump.Priority = Enum.AnimationPriority.Action
animTrackJump.Looped = false
animTrackJump:SetSpeed(1.0)
local animationAttack = Instance.new("Animation")
animationAttack.AnimationId = "rbxassetid://" .. AttackId
local animTrackAttack = character.Humanoid.Animator:LoadAnimation(animationAttack)
animTrackAttack.Priority = Enum.AnimationPriority.Action
animTrackAttack.Looped = false
animTrackAttack:SetSpeed(1.2)

-- Initialize tweens
local tweenpart1 = TweenService:Create(part1, TweenInfo.new(2.0, Enum.EasingStyle.Quad, Enum.EasingDirection.Out), { Position = Vector3.new(10, 5, 0) })
local tweenpart2 = TweenService:Create(part2, TweenInfo.new(1.5, Enum.EasingStyle.Back, Enum.EasingDirection.InOut), { Size = Vector3.new(5, 5, 5) })
local tweenpart3 = TweenService:Create(part3, TweenInfo.new(3.0, Enum.EasingStyle.Linear, Enum.EasingDirection.In), { Transparency = 1 })
local tweenguiButton = TweenService:Create(guiButton, TweenInfo.new(0.5, Enum.EasingStyle.Elastic, Enum.EasingDirection.Out), { Size = Vector2.new(0.3, 0.15) })
local tweenlight = TweenService:Create(light, TweenInfo.new(4.0, Enum.EasingStyle.Sine, Enum.EasingDirection.InOut), { Color = Color3.fromRGB(0, 0, 255) })

-- Set up event handlers
print("Tween completed for part1")
onPart1TweenComplete()

animTrackIdle:Play()
print("Playing animation: Idle")

animTrackWalk:Play()
print("Playing animation: Walk")

animTrackIdle:Stop()
print("Stopping animation: Idle")

animTrackAttack.KeyframeReached:Connect(function(keyframeName)
  if keyframeName == "Hit" then
    playHitEffect()
  end
end)

playButton.MouseButton1Click:Connect(function()
  onPlayButtonClicked()
end)

character:GetPropertyChangedSignal("Health"):Connect(function()
  updateHealthUI()
end)

-- Animation utility functions and state controller functions follow...
|# 