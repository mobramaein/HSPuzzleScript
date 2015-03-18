import qualified Data.Vector as V
--Keywords --
data Direction = Absolute AbsoluteDir | Relative RelativeDir | NoDir deriving (Eq, Show)
data RuleKeyWord = Late | Horizontal | Vertical | None deriving (Eq,Show)
data RelativeDir = Up| Down | Rgt | Lft | Perpendicular | Moving | Stationary | Parallel | NoOp deriving (Eq,Show)
data AbsoluteDir = AUp| ADown | ARgt | ALft | ANo | AEllipses | ANoOp deriving (Eq,Show)
data LogicWord = All | NoNe | On | Some deriving (Eq, Show)
data SectionNames = Objects| Legend | Sounds | CollisionLayers | Rules [Rule] | WinConditions [WinCondition] | Levels deriving(Eq, Show)
data CommandWords = Sfx0 |Sfx1 |Sfx2 |Sfx3 |Sfx4 |Sfx5 |Sfx6 |Sfx7 |Sfx8 |Sfx9 |Sfx10 | Cancel| Checkpoint | Restart |Win |Message | Again  deriving(Eq,Show)

--This is our definition of a Game Object in the world. 
--String for name, list of Strings for Colors, list of Strings for Sprite, Char for Legend Mapping, Coord for State Management -  
data GameObject = GameObject {name :: String, colors :: [String], sprite :: [String], legendID :: Char} deriving (Eq,Show)

data StateGO = StateGO { gameObject :: GameObject , x :: Int, y :: Int} deriving(Eq,Show)

--This is the type for a game rule according to the grammar for two object rules. This will be expanded for multiple objects later--
data Rule = Obj GameObject | 
			Prec Direction Rule Direction Rule |
			Post Direction Rule Direction Rule |
			GameRule RuleKeyWord Rule Rule 
			deriving (Eq,Show)

--This is the type for a Winning condition- 
--There are games with no winning conditions. 
data WinCondition = WinC LogicWord GameObject LogicWord GameObject | WinCS LogicWord GameObject deriving(Eq,Show)

--A level is a simple list of strings that will be parsed after getting the tokens from the legend. '.' is a reserved character for the background.
data Level = Level [String] 
--A state can be defined as the list of the current state of all game objects
data State = State [StateGO] 

--Take a state game object and an input and return a new temporary state with different coordinates 
updateCoord :: StateGO -> Int -> String 
updateCoord s i = if i == 1 then "Going Left" 
				else if i == 2 then "Going Right" 
				else if i == 3 then "Going Down"
				else "Going Up"

--Check if the coordinates of two objects will collide, if the coordinates are the same, then it is a collision. (Assume only one collision layer)
-- Seriously, fuck haskell. SERIOUSLY FUCK HASKELL. I CAN'T RECURSE TO SAVE MY LIFE. 
checkCollision :: StateGO -> StateGO -> Bool 
checkCollision s1 s2 =  if (s1 == s2) then True else False 

-- Loop should go like this, update the coord by applying the input, then check for collisions against everything in the state but you. apply rules on every collision. Some will be null. 
applyRuleOnce :: StateGO -> State -> Int -> Rule -> String 
applyRuleOnce s s0 i r = updateCoord s i
main :: IO() 
main = 	
		let
		--Game Object merges the Legend and the graphics in one simplified object. 
		wall = GameObject "Wall" ["BROWN"] ["00000","00000","00000","00000","00000"] '#' 
		crate = GameObject "Crate" ["GREEN"] ["00000","00000","00000","00000","00000"] 'C' 
		player = GameObject "Player" ["BLUE"] ["00000","00000","00000","00000","00000"] 'P' 
		target = GameObject "Player" ["YELLOW"] ["00000","00000","00000","00000","00000"] 'T' 
		--Now we try to put them on an environment that has not been initialized.
		wallS = StateGO wall 0 0 
		playerS = StateGO player 0 0
		crateS = StateGO crate 0 0 
		--Here is the winning condition for the game. 
		winC = WinC All crate On target
		--Here are the rules of the game.
		r1 =  GameRule None (Prec (Relative Rgt) (Obj player) (NoDir) (Obj crate)) (Post  (Relative Rgt) (Obj player) (Relative Rgt) (Obj crate))
		--Lets load a level for this game 
		l1 = ["#########","#.......#","#.....T.#","#.P.*.C.#","#.......#","#.......#","#########"]
		inputs = [1,2,4,3,2,1,3,4,3,3]
		state = State [wallS,playerS,crateS]
		in
		do
		print $ wallS
		print $ applyRuleOnce playerS state 1 r1

