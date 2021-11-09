require daslib/media
require daslib/decs_boost
require daslib/random
require daslib/apply
options debugger
require daslib/debug


[decs_template]
struct UnitsGenerator
    nextGenerateAt: float
    generationTime: float


[decs_template]
struct Selector
    startPos: float2


let
    NO_TEAM = 0
    PLAYER_TEAM = 1


struct LevelPlanet
    relPos: float2
    team: int


var
    selectorEid: decs::EntityId
    selectorColor: uint
    teamColors: array<uint>
    randomSeed = random_seed(0)
    nextGeneration: float
    frames: int
    framesCountStart: float
    aiControllers: array<AiController>
    planetsCount: int
    gameState: int
    unitSpawner: decs::EntityId
    levels: array<array<LevelPlanet>>


struct AiController
    units: array<EntityId>
    team: int
    waitForCount: int


def is_invalid_entity_id(eid: EntityId#): bool
    return eid.id == 0u && eid.generation == 0


def make_unit(pos, target: float2; radius: float; team: int)
    create_entity <| @ ( eid, cmp )
        cmp |> set("eid", eid)
        cmp |> set("pos", pos)
        cmp |> set("radius", radius)
        cmp |> set("color", teamColors[team])
        cmp |> set("isFilledCircle", true)
        cmp |> set("isSelected", false)
        cmp |> set("spd", 35f)
        cmp |> set("target", target)
        cmp |> set("targetPlanet", -1)
        cmp |> set("team", team)
        if team != PLAYER_TEAM
            for controller in aiControllers
                if controller.team == team
                    controller.units |> push(eid)
                    break


def make_planet(pos: float2; radius: float; team: int)
    create_entity <| @ ( eid, cmp )
        cmp |> set("eid", eid)
        cmp |> set("id", planetsCount++)
        cmp |> set("pos", pos)
        cmp |> set("unitsGeneratePerTick", 1)
        cmp |> set("team", team)
        cmp |> set("attackingTeam", 0)
        cmp |> set("health", team == NO_TEAM ? 0 : 50)
        cmp |> set("maxHealth", 50)
        cmp |> set("radius", radius)
        cmp |> set("color", teamColors[team])
        cmp |> set("isFilledCircle", false)


def select_units()
    query <| $ (pos: float2; radius: float; s: Selector)
        let selectorPos = pos
        let selectorRadSq = radius * radius
        query() <| $ (eid: EntityId; pos: float2; team: int; var color: uint&; var isSelected: bool&)
            if (length_sq(selectorPos - pos) <= selectorRadSq) && team == PLAYER_TEAM
                isSelected = true
                color = selectorColor
            elif isSelected
                color = teamColors[team]
                isSelected = false

    delete_entity(selectorEid)
    commit()


def move_selected_to_pos(move_target: float2; owner_team: int)
    var planetId = -1
    var planetPos: float2
    var targetIsPlanet = find_query() <| $ [REQUIRE(unitsGeneratePerTick)] (eid: EntityId; pos: float2; id, team: int; radius: float; health, maxHealth: int)
        if is_equal(pos, move_target, radius) && (team != owner_team || health != maxHealth)
            planetId = id
            planetPos = pos
            return true
    var unitCount = 0
    query() <| $ (isSelected: bool; team: int; var target: float2&; var targetPlanet: int&)
        if isSelected && team == owner_team
            unitCount += 1
            let bias = random_in_unit_disk(randomSeed)
            let biasRad = 3.0 * log2(float(unitCount))
            if targetIsPlanet
                target = planetPos + 5f * float2(bias.x, bias.y)
            else
                target = move_target + biasRad * float2(bias.x, bias.y)
            targetPlanet = planetId


[decs(stage = update)]
def generate_units(var ug: UnitsGenerator)
    nextGeneration = ug.nextGenerateAt
    if get_time_after_start() < ug.nextGenerateAt
        return
    ug.nextGenerateAt = get_time_after_start() + ug.generationTime

    query <| $ (pos: float2; team, unitsGeneratePerTick: int; radius: float)
        if team == NO_TEAM
            return
        for i in range(unitsGeneratePerTick)
            let randomVelocity = random_in_unit_disk(randomSeed)
            make_unit(pos + float2(randomVelocity.x, randomVelocity.y) * radius * 0.3, pos + float2(randomVelocity.x, randomVelocity.y) * radius, 1.5, team)


def fill_planet(planet_id: int; unit_eid: EntityId#; unit_team: int)
    find_query() <| $ (var team, attackingTeam, health: int&; id, maxHealth: int; var color: uint&)
        if id == planet_id
            if team == NO_TEAM
                if attackingTeam == NO_TEAM
                    attackingTeam = unit_team
                health += (attackingTeam == unit_team ? 1 : -1)
                if health == 0
                    attackingTeam = unit_team
                elif health == maxHealth
                    team = unit_team
                    color = teamColors[team]
                    attackingTeam = NO_TEAM
                delete_entity(unit_eid)
            elif team != unit_team
                health -= 1
                if health == 0
                    team = NO_TEAM
                    color = teamColors[NO_TEAM]
                    attackingTeam = unit_team
                delete_entity(unit_eid)
            elif health != maxHealth
                health += 1
                delete_entity(unit_eid)
            return true


def process_ai(var controller: AiController&)
    var planetsNeedHelp: array<tuple<pos:float2; needHpRestore:int>>
    var planetsToAttack: array<tuple<pos:float2; team:int; needAttackers:int>>
    find_query <| $ (eid: EntityId; pos: float2; team, health, maxHealth: int)
        if team == controller.team
            if health < maxHealth
                planetsNeedHelp |> emplace([[auto pos, maxHealth - health]])
        else
            planetsToAttack |> emplace([[auto pos, team, team == NO_TEAM ? maxHealth - health : health]])

    if planetsNeedHelp |> length() > 0
        for planet in planetsNeedHelp
            if controller.units |> length() > planet.needHpRestore / 10
                for i in range(planet.needHpRestore)
                    let lastUnit = controller.units |> length() -1
                    if lastUnit >= 0
                        query(controller.units[lastUnit]) <| $ (var isSelected: bool&)
                            isSelected = true
                        controller.units |> erase(lastUnit)
                move_selected_to_pos(planet.pos, controller.team)

    if controller.units |> length() > controller.waitForCount
        let planetToAttack = planetsToAttack[random_int(randomSeed) % length(planetsToAttack)]
        for unit in controller.units
            query(unit) <| $ (var isSelected: bool&)
                isSelected = true
        move_selected_to_pos(planetToAttack.pos, controller.team)
        controller.units |> clear()
        controller.waitForCount = 10 + random_int(randomSeed) % 35


def process_win_lose()
    var playerPlanets = 0
    var enemyPlanets = 0
    query <| $ [REQUIRE(unitsGeneratePerTick)](team: int)
        if team == PLAYER_TEAM
            playerPlanets += 1
        elif team != NO_TEAM
            enemyPlanets += 1
    if playerPlanets == 0
        gameState = 1
        delete_entity(unitSpawner)
    elif enemyPlanets == 0
        gameState = 2
        delete_entity(unitSpawner)


[decs(stage = update)]
def update_unit(eid: EntityId; var pos: float2&; spd: float; target: float2; team, targetPlanet: int)
    let dt = get_delta_time()
    let sw = float(get_screen_width())
    let sh = float(get_screen_height())
    let direction = target - pos
    let directionLen = length(direction)
    let directionDir = directionLen != 0f ? direction / directionLen : float2()
    let velocity = directionDir * min(spd, directionLen)
    pos += dt * velocity
    let unitPos = pos
    let unitTeam = team
    let isCollideOther = find_query <| $ [REQUIRE_NOT(unitsGeneratePerTick)](eid: EntityId; pos: float2; team: int)
        if unitTeam != team && is_equal(pos, unitPos, 6f)
            delete_entity(eid)
            return true
    if isCollideOther
        delete_entity(eid)
    if directionLen <= 1f && targetPlanet != -1
        fill_planet(targetPlanet, eid, team)


[decs(stage = update)]
def update_selector(var pos: float2&; var radius: float&; s: Selector)
    let newMousePos = get_mouse_position()
    pos = s.startPos + (newMousePos - s.startPos) / 2f
    radius = length(newMousePos - s.startPos) / 2f


def is_equal(pos1, pos2: float2; eps: float): bool
    return length_sq(pos1 - pos2) <= eps * eps


[decs(stage = draw)]
def draw_circle(pos: float2; radius: float; color: uint; isFilledCircle: bool)
    if isFilledCircle
        fill_circle(pos.x, pos.y, radius, color)
    else
        circle(pos.x, pos.y, radius, color)


[decs(stage = draw)]
def draw_planet(pos: float2; health, maxHealth, team, attackingTeam: int; radius: float)
    if health != maxHealth && health != 0
        rect(pos.x - radius, pos.y + radius + 5f, radius * 2f, 3f, make_color(0.7))
        let color = team == NO_TEAM ? teamColors[attackingTeam] : teamColors[team]
        fill_rect(pos.x - radius, pos.y + radius + 5f, radius * 2f * (float(health) / float(maxHealth)), 3f, color)


[export]
def initialize(is_hard_reload: bool)
    set_window_title("Neutrino Attack")
    randomize_seed(randomSeed)
    framesCountStart = 0.1

    if is_hard_reload
        unitSpawner = create_entity <| @ ( eid, cmp )
            cmp |> set("eid", eid)
            apply_decs_template(cmp, [[UnitsGenerator nextGenerateAt = get_time_after_start() + 1f, generationTime = 1f]])
        commit()

    selectorColor = make_color(0.7)
    teamColors |> emplace(make_color(0.7)) //neutral team
    teamColors |> emplace(make_color(0f, 0f, 1f))
    teamColors |> emplace(make_color(0f, 1f, 0f))
    teamColors |> emplace(make_color(1f, 0f, 0f))

    if is_hard_reload
        levels |> clear()
        var level1: array<LevelPlanet>
        level1 |> emplace([[LevelPlanet relPos=float2(0.25, 0.5), team=PLAYER_TEAM]])
        level1 |> emplace([[LevelPlanet relPos=float2(0.5, 0.5), team=NO_TEAM]])
        level1 |> emplace([[LevelPlanet relPos=float2(0.75, 0.5), team=PLAYER_TEAM+1]])
        levels |> emplace(level1)
        var level2: array<LevelPlanet>
        level2 |> emplace([[LevelPlanet relPos=float2(0.25, 0.5), team=PLAYER_TEAM]])
        level2 |> emplace([[LevelPlanet relPos=float2(0.5, 0.25), team=NO_TEAM]])
        level2 |> emplace([[LevelPlanet relPos=float2(0.5, 0.75), team=NO_TEAM]])
        level2 |> emplace([[LevelPlanet relPos=float2(0.75, 0.5), team=PLAYER_TEAM+1]])
        levels |> emplace(level2)
        var level3: array<LevelPlanet>
        level3 |> emplace([[LevelPlanet relPos=float2(0.25, 0.5), team=PLAYER_TEAM]])
        level3 |> emplace([[LevelPlanet relPos=float2(0.5, 0.5), team=NO_TEAM]])
        level3 |> emplace([[LevelPlanet relPos=float2(0.75, 0.75), team=PLAYER_TEAM+1]])
        level3 |> emplace([[LevelPlanet relPos=float2(0.75, 0.25), team=PLAYER_TEAM+2]])
        levels |> emplace(level3)

        let level& = levels[random_int(randomSeed) % length(levels)]
        var maxTeam = 0
        for planet in level
            make_planet(float2(float(get_screen_width()) * planet.relPos.x, float(get_screen_height()) * planet.relPos.y), 25.0, planet.team)
            maxTeam = max(maxTeam, planet.team)
        commit()
        for i in range(maxTeam + 1)
            if i > PLAYER_TEAM
                print("add ai {i}")
                aiControllers |> emplace([[AiController team = i, waitForCount = 10 + random_int(randomSeed) % 15]])


[export]
def act(dt: float)
    if get_key(VK_ESCAPE)
        schedule_quit_game()

    if get_mouse_button_down(MB_LEFT)
        if selectorEid.id != 0u
            delete_entity(selectorEid)
        selectorEid = create_entity <| @ ( eid, cmp )
            cmp |> set("eid", eid)
            cmp |> set("pos", get_mouse_position())
            cmp |> set("radius", 1f)
            cmp |> set("color", selectorColor)
            cmp |> set("isFilledCircle", false)
            apply_decs_template(cmp, [[Selector startPos = get_mouse_position()]])
        commit()
    elif get_mouse_button_up(MB_LEFT)
        if selectorEid.id != 0u
            select_units()

    if get_mouse_button_down(MB_RIGHT)
        move_selected_to_pos(get_mouse_position(), PLAYER_TEAM)

    process_win_lose()

    for controller in aiControllers
        process_ai(controller)

    decs_stage("update")


[export]
def draw
    decs_stage("draw")
    frames += 1
    let fps = float(frames)/(get_time_after_start() - framesCountStart)
    if frames > 100
        frames = 0
        framesCountStart = get_time_after_start()
    if gameState != 0
        text_out(20, 20, "You {gameState == 1 ? "Lose" : "Win"}. Ctrl+F5 to new game", gameState == 1 ? 0xFF0000 : 0x00FF00)
    else
        text_out(20, 20, "Capture all enemy planets with your neutrinos to win", 0xFFFFFF)
    text_out(20, 40, "LMB - select, RMB - move", 0xFFFFFF)
    text_out(20, 60, "Fps {int(fps)}", 0xFFFFFF)
