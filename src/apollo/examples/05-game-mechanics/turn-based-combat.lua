-- Module: example
-- Language: racket

character = name(hp, max-hp, attack, defense, magic, items)

monster = name(hp, max-hp, attack, defense, weakness)

combat-state = character(monsters, turn, log)

function make-character(name, attack, defense, magic)
  return character(name, 100, 100, attack, defense, magic, quote({}))
end

function make-monster(name, hp, attack, defense, weakness)
  return monster(name, hp, hp, attack, defense, weakness)
end

define(character = monsters, combat-state(character, monsters, 0, quote({})))

define(state = message, struct-copy(combat-state, state, log(message = combat-state-log(state))))

define(state = action, match(action, quasiquote(attack(unquote(target-index)))(let*(char(combat-state-character(state))(monsters(combat-state-monsters(state)), target(monsters = target-index), damage(max(1, -(character-attack(char), monster-defense(target)))), new-hp(max(0, -(monster-hp(target), damage))), updated-target(struct-copy(monster, target, hp(new-hp))), updated-monsters(list-set(monsters, target-index, updated-target)), log-message(format("~a attacks ~a for ~a damage!", character-name(char), monster-name(target), damage)), state-with-log(state = log-message)), struct-copy(combat-state, state-with-log, monsters(updated-monsters), turn(add1(combat-state-turn(state)))))), quasiquote(cast(unquote(spell-name), unquote(target-index)))(let*(char(combat-state-character(state))(monsters(combat-state-monsters(state)), target(monsters = target-index), base-damage(character-magic(char)), weakness-multiplier(if(spell-name = monster-weakness(target), 2.0, 1.0)), damage(exact-floor(base-damage = weakness-multiplier)), new-hp(max(0, -(monster-hp(target), damage))), updated-target(struct-copy(monster, target, hp(new-hp))), updated-monsters(list-set(monsters, target-index, updated-target)), weakness-msg(if(weakness-multiplier = 1.0, " It's super effective!", "")), log-message(format("~a casts ~a on ~a for ~a damage!~a", character-name(char), spell-name, monster-name(target), damage, weakness-msg)), state-with-log(state = log-message)), struct-copy(combat-state, state-with-log, monsters(updated-monsters), turn(add1(combat-state-turn(state)))))), quasiquote(use-item(unquote(item-name)))(let*(char(combat-state-character(state))(items(character-items(char)), has-item?(item-name = items), state(if(has-item?, state, state = format("~a doesn't have ~a in inventory!", character-name(char), item-name)))), if(has-item?, match(item-name, "potion"(let*(heal-amount(30)(new-hp(min(character-max-hp(char), +(character-hp(char), heal-amount))), updated-char(struct-copy(character, char, hp(new-hp), items(item-name = items))), log-message(format("~a uses a potion and recovers ~a HP!", character-name(char), heal-amount)), state-with-log(state = log-message)), struct-copy(combat-state, state-with-log, character(updated-char), turn(add1(combat-state-turn(state)))))), "bomb"(let*(damage(15)(monsters(combat-state-monsters(state)), updated-monsters(map(lambda(m(), struct-copy(monster, m, hp(max(0, -(monster-hp(m), damage))))), monsters)), updated-char(struct-copy(character, char, items(item-name = items))), log-message(format("~a throws a bomb for ~a damage to all enemies!", character-name(char), damage)), state-with-log(state = log-message)), struct-copy(combat-state, state-with-log, character(updated-char), monsters(updated-monsters), turn(add1(combat-state-turn(state)))))), _(state)), state))), _(state = "Invalid action!")))

function monster-turn(state)
  return let*(char(combat-state-character(state))(monsters(combat-state-monsters(state)), living-monsters(filter(lambda(m(), >(monster-hp(m), 0)), monsters))), if(null?(living-monsters), state, foldl(lambda(monster(state), let*(damage(max(1, -(monster-attack(monster), character-defense(char))))(new-hp(max(0, -(character-hp(char), damage))), updated-char(struct-copy(character, char, hp(new-hp))), log-message(format("~a attacks ~a for ~a damage!", monster-name(monster), character-name(char), damage)), state-with-log(state = log-message)), struct-copy(combat-state, state-with-log, character(updated-char)))), state, living-monsters)))
end

function combat-over?(state)
  return let*(char(combat-state-character(state))(monsters(combat-state-monsters(state)), char-dead?(<=(character-hp(char), 0)), monsters-dead?(andmap(lambda(m(), <=(monster-hp(m), 0)), monsters))), char-dead? = monsters-dead?)
end

function combat-result(state)
  return let*(char(combat-state-character(state))(monsters(combat-state-monsters(state)), char-dead?(<=(character-hp(char), 0)), monsters-dead?(andmap(lambda(m(), <=(monster-hp(m), 0)), monsters))), cond(char-dead?(quote(monsters-win)), monsters-dead?(quote(player-wins)), else(quote(combat-ongoing))))
end

function run-sample-combat()
  return let*(hero(make-character("Hero", 20, 10, 25))(hero-with-items(struct-copy(character, hero, items(quote("potion"("potion", "bomb"))))), monsters(list(make-monster("Goblin", 40, 15, 5, quote(fire)), make-monster("Skeleton", 60, 12, 8, quote(light)), make-monster("Slime", 30, 10, 3, quote(ice)))), initial-state(hero-with-items = monsters), actions(list(quote(attack(0)), quote(fire = 0), quote(use-item("potion")), quote(attack(1)), quote(light = 1), quote(use-item("bomb")), quote(attack(2)), quote(ice = 2)))), let(loop, state(initial-state)(remaining-actions(actions)), printf("Turn ~a:~n", combat-state-turn(state)), display-combat-state(state), cond(combat-over?(state)(printf("~nCombat over! Result: ~a~n", combat-result(state)), display-combat-log(state)), null?(remaining-actions)(printf("~nNo more actions. Combat continues...~n")), else(let*(action(table.first(remaining-actions))(state-after-action(state = action), state-after-monster-turn(monster-turn(state-after-action))), printf("~nAction: ~a~n", action), display-combat-log(state-after-monster-turn), state-after-monster-turn = (function(t) local result = {}; for i=2,#t do result[i-1] = t[i] end; return result; end)(remaining-actions))))))
end

function display-combat-state(state)
  return let(char(combat-state-character(state))(monsters(combat-state-monsters(state))), printf("~a: HP ~a/~a, ATK ~a, DEF ~a, MAG ~a~n", character-name(char), character-hp(char), character-max-hp(char), character-attack(char), character-defense(char), character-magic(char)), printf("Items: ~a~n", character-items(char)), printf("Monsters:~n"), for(m(monsters)(i(in-naturals())), printf("  ~a. ~a: HP ~a/~a, ATK ~a, DEF ~a, Weakness: ~a~n", i, monster-name(m), monster-hp(m), monster-max-hp(m), monster-attack(m), monster-defense(m), monster-weakness(m))))
end

function display-combat-log(state)
  return let(log(reverse(combat-state-log(state)))(), printf("Combat log:~n"), for(msg(log)(), printf("  ~a~n", msg)))
end

run-sample-combat()